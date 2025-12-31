/**
 * Application configuration from environment variables.
 *
 * In development: Uses Vite proxy ("/api")
 * In production: Set VITE_API_URL to your Worker URL
 *
 * Example .env.production:
 *   VITE_API_URL=https://tidepool.your-subdomain.workers.dev
 */

/** Base URL for HTTP API calls (session creation) */
export const API_URL = import.meta.env.VITE_API_URL || "/api";

/** Whether we're in development mode */
export const IS_DEV = import.meta.env.DEV;
