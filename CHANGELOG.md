# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [0.1.0] - 2026-02-24

### Added
- Agent orchestration: `spawn_subtree` (Claude), `spawn_leaf_subtree` (Gemini), `spawn_workers` (Gemini panes)
- PR workflow: `file_pr`, `merge_pr`, `notify_parent`
- Haskell WASM effect system with typed effects and Rust host handlers
- Hot reload for WASM tools in HTTP serve mode
- Push-based agent coordination via Zellij stdin injection
- Zellij plugin for agent status display and interactive popup UI
- Event logging (JSONL) and GitHub poller for CI/review status
- Role system: TL, Dev, Worker roles with permission cascades
- `exomonad init` for session bootstrap (server tab + TL tab)
