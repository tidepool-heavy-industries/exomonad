# Gemini Code Intelligence File

This file provides context for Gemini to understand the Tidepool project.

## Project Overview

Tidepool is a framework for building large language model (LLM) agents using a typed, functional approach in Haskell. It emphasizes safety and predictability by treating agents as state machines that operate within a controlled environment.

The core principles of Tidepool are:

*   **Type-Safe State:** Agents interact with a well-defined, typed state, which is manipulated through structured, auditable mutations.
*   **Sandboxed Effects:** Agents are restricted in their capabilities. They cannot perform arbitrary I/O operations. Instead, they use a system of "effects" (inspired by `freer-simple`) to request services from the environment, such as making LLM calls, requesting user input, or interacting with external APIs.
*   **Declarative Templates:** LLM prompts are generated using the Ginger templating engine (a Haskell implementation of Jinja). This allows for clear separation between the agent's logic and the presentation of information to the LLM. Templates are validated at compile time to ensure they are consistent with the agent's data context.
*   **Structured Output:** The framework expects LLMs to return structured data (e.g., JSON) that can be decoded into precise, typed state transitions. This avoids the fragility of parsing natural language responses.

The project is a monorepo containing several Haskell packages, a TypeScript-based Cloudflare Worker for deployment, and various tools and documentation.

## Building and Running

The project uses `cabal` for building Haskell code and `just` as a command runner for common development tasks.

### Key Commands

*   **Build all packages:**
    ```bash
    just build
    ```
    or
    ```bash
    cabal build all
    ```

*   **Run all tests:**
    ```bash
    just test
    ```
    or
    ```bash
    cabal test all
    ```

*   **Run the native server (for local development):**
    ```bash
    just native
    ```
    This starts a web server on `localhost:8080` that can be used to interact with agents.

*   **Lint the codebase:**
    ```bash
    just lint
    ```
    This runs `hlint` on the Haskell code and `eslint` on the TypeScript code.

*   **Deploy to Cloudflare Workers:**
    ```bash
    just deploy
    ```
    This command builds the WASM artifact and deploys it to Cloudflare.

For a full list of available commands, run `just` with no arguments.

## Development Conventions

*   **Haskell:** The project uses modern Haskell (GHC 9.6+) with the `GHC2021` language edition. It follows a functional style, with a strong emphasis on types and effects.
*   **TypeScript:** The TypeScript code in the `deploy/` directory is used for the Cloudflare Worker. It is linted with `eslint` and tested with `vitest`.
*   **Git Hooks:** The project provides a pre-commit hook that runs `just pre-commit` (which runs `build`, `lint`, and `test`). To install the hook, run:
    ```bash
    just install-hooks
    ```

## Project Structure

*   `haskell/`: Contains the core Haskell packages.
    *   `dsl/core/`: The core domain-specific language for defining agents.
    *   `runtime/`: The runtime environment for executing agents.
    *   `effects/`: Interpreters for the various effects that agents can use.
    *   `native-server/`: The web server for running agents locally.
    *   `protocol/`: Defines the wire protocol for communication between the agent and the environment.
*   `deploy/`: The Cloudflare Worker for deploying agents.
*   `anemone/`: A separate project that seems to be a client-side application for interacting with the Tidepool agents.
*   `justfile`: Defines the `just` commands.
*   `cabal.project`: The Cabal project file, which defines the packages in the workspace.
*   `README.md`: The main README file, which provides a high-level overview of the project.
*   `CLAUDE.md`: A more detailed document about the project, likely with more in-depth architectural details.
*   `flake.nix`: Indicates that the project uses Nix for reproducible development environments.
*   `rust/`: Contains Rust code, but its role in the project is not immediately clear from the other files.
*   `typescript/`: Contains TypeScript code, likely for the frontend or other related tooling.

This file should be kept up-to-date as the project evolves.
