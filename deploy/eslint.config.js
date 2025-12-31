import eslint from "@eslint/js";
import tseslint from "@typescript-eslint/eslint-plugin";
import tsparser from "@typescript-eslint/parser";

export default [
  eslint.configs.recommended,
  {
    files: ["src/**/*.ts"],
    languageOptions: {
      parser: tsparser,
      parserOptions: {
        project: "./tsconfig.json",
      },
      // CF Workers globals - TypeScript handles these via @cloudflare/workers-types
      globals: {
        console: "readonly",
        fetch: "readonly",
        crypto: "readonly",
        setTimeout: "readonly",
        queueMicrotask: "readonly",
        URL: "readonly",
        Request: "readonly",
        Response: "readonly",
        WebSocket: "readonly",
        WebSocketPair: "readonly",
        WebAssembly: "readonly",
        TextEncoder: "readonly",
        TextDecoder: "readonly",
        TextDecoderConstructorOptions: "readonly",
        Ai: "readonly",
        DurableObjectNamespace: "readonly",
      },
    },
    plugins: {
      "@typescript-eslint": tseslint,
    },
    rules: {
      // TypeScript-specific rules
      ...tseslint.configs.recommended.rules,
      "@typescript-eslint/no-unused-vars": ["error", { argsIgnorePattern: "^_" }],
      "@typescript-eslint/no-explicit-any": "warn",

      // Disable no-undef - TypeScript handles this better
      "no-undef": "off",

      // General quality rules
      "no-console": "off", // We use console for logging
      "prefer-const": "error",
      "no-var": "error",
      "eqeqeq": ["error", "always"],
      "no-throw-literal": "error",
    },
  },
  {
    ignores: ["node_modules/", "*.mjs"],
  },
];
