{-# LANGUAGE OverloadedStrings #-}
-- | Extract variable access paths from ginger template AST.
-- Performs scope-aware traversal to distinguish free variables
-- (that must come from context) from locally bound variables.
-- Also tracks narrowing context from @is defined@ guards.
module Text.Ginger.TH.Extract
  ( extractVariableAccesses
  , extractFromTemplate
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Control.Monad (foldM)
import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Text.Parsec.Pos (SourcePos)

import Text.Ginger.AST
import Text.Ginger.TH.Types (AccessPath(..), PathSegment(..), NarrowedPath(..))

-- | Local scope: set of variable names that are bound locally.
type LocalScope = Set Text

-- | Set of paths that have been narrowed (guarded by @is defined@).
type NarrowedPaths = Set NarrowedPath

-- | Extract all variable access paths from a template.
-- Returns paths for free variables only (not locally bound ones).
extractFromTemplate :: Template SourcePos -> [AccessPath]
extractFromTemplate tpl = extractVariableAccesses (templateBody tpl)

-- | Extract variable accesses from a statement.
extractVariableAccesses :: Statement SourcePos -> [AccessPath]
extractVariableAccesses stmt = execWriter (walkStatement Set.empty Set.empty stmt)

-- | Walk a statement, collecting access paths.
-- The scope parameter tracks locally bound variables.
-- The narrowed parameter tracks paths guarded by @is defined@.
-- Returns the updated scope (for sequential binding via SetVarS).
walkStatement :: LocalScope -> NarrowedPaths -> Statement SourcePos -> Writer [AccessPath] LocalScope
walkStatement scope narrowed stmt = case stmt of
  MultiS _ stmts ->
    -- Process statements sequentially, threading scope through
    -- so that SetVarS bindings are visible to subsequent statements
    foldM (\s st -> walkStatement s narrowed st) scope stmts

  ScopedS _ body -> do
    -- ScopedS creates isolated scope - bindings don't escape
    _ <- walkStatement scope narrowed body
    return scope

  IndentS _ expr body -> do
    walkExpression scope narrowed expr
    walkStatement scope narrowed body

  LiteralS _ _ ->
    return scope

  InterpolationS _ expr -> do
    walkExpression scope narrowed expr
    return scope

  ExpressionS _ expr -> do
    walkExpression scope narrowed expr
    return scope

  IfS _ cond trueBranch falseBranch -> do
    walkExpression scope narrowed cond
    -- Extract narrowing from the condition
    let trueNarrowed = narrowed `Set.union` extractNarrowing cond
    let falseNarrowed = narrowed `Set.union` extractNarrowingForFalse cond
    -- Branches see narrowed paths; bindings don't escape
    _ <- walkStatement scope trueNarrowed trueBranch
    _ <- walkStatement scope falseNarrowed falseBranch
    return scope

  SwitchS _ expr cases defaultCase -> do
    walkExpression scope narrowed expr
    mapM_ (\(caseExpr, caseBody) -> do
      walkExpression scope narrowed caseExpr
      walkStatement scope narrowed caseBody) cases
    _ <- walkStatement scope narrowed defaultCase
    return scope

  ForS _ mIndex varName iterExpr body -> do
    -- The iterable expression is evaluated in outer scope
    walkExpression scope narrowed iterExpr
    -- For loop binds: index (optional), value, and implicit "loop"
    let scope' = scope
          `Set.union` Set.singleton varName
          `Set.union` maybe Set.empty Set.singleton mIndex
          `Set.union` Set.singleton "loop"
    -- Narrowing from outside applies inside loop
    _ <- walkStatement scope' narrowed body
    return scope

  SetVarS _ varName expr -> do
    -- RHS is evaluated in current scope (before binding)
    walkExpression scope narrowed expr
    -- Return scope with the new binding for subsequent statements
    return $ Set.insert varName scope

  DefMacroS _ macroName (Macro argNames body) -> do
    -- Macro body has its own scope with just the arguments
    -- Narrowing context does NOT cross into macro body (isolated)
    let macroScope = Set.fromList argNames
          `Set.union` Set.singleton "varargs"
          `Set.union` Set.singleton "kwargs"
    _ <- walkStatement macroScope Set.empty body
    -- The macro name is now available in scope
    return $ Set.insert macroName scope

  BlockRefS _ _ ->
    return scope

  PreprocessedIncludeS _ includedTpl -> do
    -- Included templates share scope and narrowing context
    walkStatement scope narrowed (templateBody includedTpl)

  NullS _ ->
    return scope

  TryCatchS _ tryBody catches finallyBody -> do
    -- Bindings in try/catch/finally don't escape
    _ <- walkStatement scope narrowed tryBody
    mapM_ (walkCatch scope narrowed) catches
    _ <- walkStatement scope narrowed finallyBody
    return scope

-- | Walk a catch block.
walkCatch :: LocalScope -> NarrowedPaths -> CatchBlock SourcePos -> Writer [AccessPath] ()
walkCatch scope narrowed (Catch _ captureVar body) = do
  -- Catch block may bind the exception variable
  let scope' = scope `Set.union` maybe Set.empty Set.singleton captureVar
  _ <- walkStatement scope' narrowed body
  return ()

-- | Walk an expression, collecting access paths.
walkExpression :: LocalScope -> NarrowedPaths -> Expression SourcePos -> Writer [AccessPath] ()
walkExpression scope narrowed expr = case expr of
  StringLiteralE _ _ -> return ()
  NumberLiteralE _ _ -> return ()
  BoolLiteralE _ _ -> return ()
  NullLiteralE _ -> return ()

  VarE pos varName
    | varName `Set.member` scope -> return ()  -- Locally bound
    | otherwise -> tell [AccessPath varName [] pos narrowed False]  -- Free variable

  ListE _ exprs ->
    mapM_ (walkExpression scope narrowed) exprs

  ObjectE _ pairs ->
    mapM_ (\(k, v) -> walkExpression scope narrowed k >> walkExpression scope narrowed v) pairs

  MemberLookupE _ _ _ ->
    -- Try to collect the full access path
    case collectAccessPath expr of
      Just (rootVar, segments, rootPos)
        | rootVar `Set.notMember` scope ->
            tell [AccessPath rootVar segments rootPos narrowed False]
        | otherwise ->
            -- Root is locally bound, but we should still walk
            -- for any dynamic key expressions
            walkMemberChainForDynamicKeys scope narrowed expr
      Nothing ->
        -- Complex base expression, walk recursively
        walkMemberChainRecursive scope narrowed expr

  CallE _ funcExpr args -> do
    walkExpression scope narrowed funcExpr
    mapM_ (walkExpression scope narrowed . snd) args

  LambdaE _ argNames body -> do
    -- Lambda params are bound in body
    let scope' = scope `Set.union` Set.fromList argNames
    walkExpression scope' narrowed body

  TernaryE _ cond true false -> do
    -- Similar to IfS, but for expressions
    walkExpression scope narrowed cond
    let trueNarrowed = narrowed `Set.union` extractNarrowing cond
    let falseNarrowed = narrowed `Set.union` extractNarrowingForFalse cond
    walkExpression scope trueNarrowed true
    walkExpression scope falseNarrowed false

  DoE _ stmt -> do
    -- Do expressions contain statements; bindings don't escape
    _ <- walkStatement scope narrowed stmt
    return ()

  IsDefinedE _ _isDefined innerExpr ->
    -- Extract paths from IsDefinedE for validation, but mark them as existence checks.
    -- The inner expression is queried for existence, not accessed for its value.
    -- We validate the path prefix (all but last segment) to catch typos early.
    -- e.g., `user.profile.name is defined` validates that user and user.profile exist.
    case collectAccessPath innerExpr of
      Just (rootVar, segments, rootPos)
        | rootVar `Set.notMember` scope ->
            -- Mark as existence check so validation only checks prefix
            tell [AccessPath rootVar segments rootPos narrowed True]
        | otherwise -> return ()  -- Root is locally bound
      Nothing -> return ()  -- Complex expression, can't validate

-- | Try to collect a chain of member lookups into a single AccessPath.
-- Returns (rootVarName, pathSegments, rootPosition) if successful.
collectAccessPath :: Expression SourcePos -> Maybe (Text, [PathSegment], SourcePos)
collectAccessPath expr = go expr []
  where
    go (VarE pos varName) segments =
      Just (varName, segments, pos)
    go (MemberLookupE _ baseExpr keyExpr) segments = do
      segment <- toSegment keyExpr
      go baseExpr (segment : segments)
    go _ _ = Nothing

    toSegment (StringLiteralE _ s) = Just (StaticKey s)
    toSegment _ = Just DynamicKey  -- Any non-literal is dynamic

-- | Walk a member lookup chain recursively when we can't collect it as a path.
walkMemberChainRecursive :: LocalScope -> NarrowedPaths -> Expression SourcePos -> Writer [AccessPath] ()
walkMemberChainRecursive scope narrowed (MemberLookupE _ baseExpr keyExpr) = do
  walkExpression scope narrowed baseExpr
  walkExpression scope narrowed keyExpr
walkMemberChainRecursive scope narrowed expr =
  walkExpression scope narrowed expr

-- | Walk member chain looking for dynamic key expressions to traverse.
walkMemberChainForDynamicKeys :: LocalScope -> NarrowedPaths -> Expression SourcePos -> Writer [AccessPath] ()
walkMemberChainForDynamicKeys scope narrowed (MemberLookupE _ baseExpr keyExpr) = do
  walkMemberChainForDynamicKeys scope narrowed baseExpr
  -- Walk the key expression if it's not a literal
  case keyExpr of
    StringLiteralE _ _ -> return ()
    _ -> walkExpression scope narrowed keyExpr
walkMemberChainForDynamicKeys _ _ (VarE _ _) = return ()
walkMemberChainForDynamicKeys scope narrowed expr = walkExpression scope narrowed expr

-- | Extract paths that are narrowed (guarded) by an @is defined@ condition.
-- Returns paths that are known to be defined in the true branch.
--
-- Note: Boolean operators are represented as CallE in ginger's AST:
--   - "and" / "&&" -> CallE (VarE "all") [left, right]
--   - "or" / "||"  -> CallE (VarE "any") [left, right]
--   - "not"        -> CallE (VarE "not") [inner]
extractNarrowing :: Expression SourcePos -> NarrowedPaths
extractNarrowing expr = case expr of
  IsDefinedE _ True inner ->
    -- "x is defined" narrows x in trueBranch
    case collectAccessPath inner of
      Just (root, segments, _) -> Set.singleton (NarrowedPath root segments)
      Nothing -> Set.empty

  IsDefinedE _ False _ ->
    -- "x is undefined" narrows nothing in trueBranch
    Set.empty

  -- "not expr" = swap true/false narrowing
  CallE _ (VarE _ funcName) [(_, inner)]
    | funcName == "not" -> extractNarrowingForFalse inner

  -- "a and b" narrows both in trueBranch
  CallE _ (VarE _ funcName) args
    | funcName == "all" ->
        Set.unions $ map (extractNarrowing . snd) args

  -- "a or b" narrows nothing (conservative)
  CallE _ (VarE _ funcName) _
    | funcName == "any" -> Set.empty

  _ -> Set.empty

-- | Extract paths that are narrowed in the false branch of a condition.
extractNarrowingForFalse :: Expression SourcePos -> NarrowedPaths
extractNarrowingForFalse expr = case expr of
  IsDefinedE _ True _ ->
    -- "x is defined" narrows nothing in falseBranch
    Set.empty

  IsDefinedE _ False inner ->
    -- "x is undefined" narrows x in falseBranch
    case collectAccessPath inner of
      Just (root, segments, _) -> Set.singleton (NarrowedPath root segments)
      Nothing -> Set.empty

  -- "not expr" = swap true/false narrowing
  CallE _ (VarE _ funcName) [(_, inner)]
    | funcName == "not" -> extractNarrowing inner

  -- "a and b" -> falseBranch means at least one is false
  -- Conservative: don't narrow anything
  CallE _ (VarE _ funcName) _
    | funcName == "all" -> Set.empty

  -- "a or b" -> falseBranch means both are false
  -- Both are narrowed in falseBranch
  CallE _ (VarE _ funcName) args
    | funcName == "any" ->
        Set.unions $ map (extractNarrowingForFalse . snd) args

  _ -> Set.empty
