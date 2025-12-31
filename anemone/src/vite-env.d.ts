/// <reference types="vite/client" />

interface ImportMetaEnv {
  /** Base URL for the Tidepool Worker API */
  readonly VITE_API_URL?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
