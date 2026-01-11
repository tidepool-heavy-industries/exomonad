import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";
import { resolve } from "path";

export default defineConfig({
  plugins: [tailwindcss()],
  build: {
    outDir: "../static",
    emptyOutDir: true,
    rollupOptions: {
      input: {
        main: resolve(__dirname, "index.html"),
        session: resolve(__dirname, "session.html"),
      },
    },
  },
  server: {
    port: 3000,
    proxy: {
      "/api": {
        target: "http://localhost:7433",
        changeOrigin: true,
      },
      "/ws": {
        target: "ws://localhost:7433",
        ws: true,
        changeOrigin: true,
      },
    },
  },
});
