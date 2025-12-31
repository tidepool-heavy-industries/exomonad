# Task B: TypeScript Effect Handlers

Implement the effect execution layer in the Cloudflare Worker.

## The Problem

When WASM yields an effect (LlmComplete, HttpFetch, Log), TypeScript needs to execute it and send back the result. Currently this is stubbed.

## Deliverables

1. **Create handler registry** - `deploy/src/handlers/index.ts`:
   ```typescript
   import { SerializableEffect, EffectResult } from '../protocol';

   export type EffectHandler = (
     effect: SerializableEffect,
     env: Env,
     ctx: ExecutionContext
   ) => Promise<EffectResult>;

   export const handlers: Record<string, EffectHandler> = {
     LogInfo: handleLogInfo,
     LogError: handleLogError,
     LlmComplete: handleLlmComplete,
     HttpFetch: handleHttpFetch,
   };

   export async function executeEffect(
     effect: SerializableEffect,
     env: Env,
     ctx: ExecutionContext
   ): Promise<EffectResult> {
     const handler = handlers[effect.type];
     if (!handler) {
       return { type: 'EffectError', err_message: `Unknown effect: ${effect.type}` };
     }
     try {
       return await handler(effect, env, ctx);
     } catch (e) {
       return { type: 'EffectError', err_message: String(e), err_requestId: effect.eff_requestId };
     }
   }
   ```

2. **Implement handlers**:
   - `handlers/log.ts`: LogInfo/LogError → console.log/error + optional DO storage
   - `handlers/llm.ts`: LlmComplete → CF AI binding
     ```typescript
     export async function handleLlmComplete(effect: LlmCompleteEffect, env: Env): Promise<EffectResult> {
       const ai = env.AI;  // CF AI binding
       const response = await ai.run(effect.eff_model, {
         prompt: effect.eff_prompt,
         max_tokens: effect.eff_maxTokens ?? 256,
       });
       return {
         type: 'LlmResult',
         res_completion: response.response,
         res_requestId: effect.eff_requestId,
       };
     }
     ```
   - `handlers/http.ts`: HttpFetch → fetch() with timeout

3. **Wire into index.ts**: Replace stub effect handling with `executeEffect()`

4. **Error handling**:
   - CF AI rate limited? Return typed error, don't crash
   - Network timeout? Return typed error with context
   - Always include requestId in errors when available

5. **Tests**: Add vitest tests for each handler (mock CF AI)

## Technical Notes

- CF AI binding is `env.AI` - needs `[[ai]]` in wrangler.toml
- Look at `deploy/src/protocol.ts` for type definitions
- Worker has 30s CPU time limit - set fetch timeouts appropriately
- requestId must be echoed back for correlation

## Holistic Improvements

- Update `deploy/CLAUDE.md` with handler documentation
- Add request logging for debugging
- Consider retry logic for transient failures
- Type the env properly (Env interface with AI binding)

Run: `cd deploy && pnpm typecheck && pnpm test`
File a PR when done.
