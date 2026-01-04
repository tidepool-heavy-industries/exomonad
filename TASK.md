# Schema Format Switching Work Stream

## Context

Different LLM providers use different formats for tool definitions and structured output schemas:
- **Anthropic** - Uses `input_schema` for tools, specific structured output format
- **Cloudflare AI** - Uses OpenAI-style `type: "function", function: {parameters}` format

The executor needs to derive the correct format based on context (which provider is being used).

## Current State

- LLM executor: `tidepool-native-gui/llm-executor/src/Tidepool/LLM/Executor.hs`
- Type-level provider switching via `SProvider` singleton (SAnthropic, SOpenAI)
- Effect definition: `tidepool-core/src/Tidepool/Effects/LLMProvider.hs`

## Key Questions

1. How does tool definition format differ between providers?
2. How does structured output schema format differ?
3. Can we derive the correct format from the provider type at compile time?
4. How does this interact with the WASM/CF AI path vs native Anthropic path?

## Key Files

- `tidepool-native-gui/llm-executor/src/Tidepool/LLM/Executor.hs` - Native interpreter
- `tidepool-native-gui/llm-executor/src/Tidepool/LLM/Types.hs` - Config types
- `tidepool-core/src/Tidepool/Effects/LLMProvider.hs` - Effect definition with provider types
- `deploy/src/handlers/llm.ts` - CF AI handler (for reference)

## Goal

Ensure schema format switching works correctly for both tool definitions and structured output across providers.
