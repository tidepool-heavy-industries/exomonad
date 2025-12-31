/**
 * Tests for HTTP effect handler.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { handleHttpFetch } from "../http.js";
import type { HttpFetchEffect } from "../../protocol.js";

// Store original fetch
const originalFetch = globalThis.fetch;

describe("handleHttpFetch", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    globalThis.fetch = originalFetch;
    vi.useRealTimers();
  });

  const baseEffect: HttpFetchEffect = {
    type: "HttpFetch",
    eff_url: "https://api.example.com/data",
    eff_method: "GET",
  };

  it("fetches JSON response", async () => {
    const mockData = { foo: "bar" };
    globalThis.fetch = vi.fn().mockResolvedValue({
      status: 200,
      headers: new Headers([["content-type", "application/json"]]),
      json: () => Promise.resolve(mockData),
    } as unknown as Response);

    const result = await handleHttpFetch(baseEffect);

    expect(result).toEqual({
      type: "success",
      value: { status: 200, body: mockData },
    });
    expect(globalThis.fetch).toHaveBeenCalledWith(
      "https://api.example.com/data",
      expect.objectContaining({ method: "GET" })
    );
  });

  it("fetches text response", async () => {
    globalThis.fetch = vi.fn().mockResolvedValue({
      status: 200,
      headers: new Headers([["content-type", "text/plain"]]),
      text: () => Promise.resolve("Hello, world!"),
    } as unknown as Response);

    const result = await handleHttpFetch(baseEffect);

    expect(result).toEqual({
      type: "success",
      value: { status: 200, body: "Hello, world!" },
    });
  });

  it("handles missing content-type as text", async () => {
    globalThis.fetch = vi.fn().mockResolvedValue({
      status: 200,
      headers: new Headers(),
      text: () => Promise.resolve("<html>...</html>"),
    } as unknown as Response);

    const result = await handleHttpFetch(baseEffect);

    expect(result).toEqual({
      type: "success",
      value: { status: 200, body: "<html>...</html>" },
    });
  });

  it("handles POST method", async () => {
    const postEffect: HttpFetchEffect = {
      type: "HttpFetch",
      eff_url: "https://api.example.com/submit",
      eff_method: "POST",
    };
    globalThis.fetch = vi.fn().mockResolvedValue({
      status: 201,
      headers: new Headers([["content-type", "application/json"]]),
      json: () => Promise.resolve({ id: 123 }),
    } as unknown as Response);

    const result = await handleHttpFetch(postEffect);

    expect(result).toEqual({
      type: "success",
      value: { status: 201, body: { id: 123 } },
    });
    expect(globalThis.fetch).toHaveBeenCalledWith(
      "https://api.example.com/submit",
      expect.objectContaining({ method: "POST" })
    );
  });

  it("returns error on network failure", async () => {
    globalThis.fetch = vi.fn().mockRejectedValue(new Error("fetch failed: network error"));

    const result = await handleHttpFetch(baseEffect);

    expect(result).toEqual({
      type: "error",
      message: "HTTP network error: fetch failed: network error",
    });
  });

  it("returns timeout error on abort", async () => {
    globalThis.fetch = vi.fn().mockImplementation(() => {
      const error = new Error("Aborted");
      error.name = "AbortError";
      return Promise.reject(error);
    });

    const result = await handleHttpFetch(baseEffect, undefined, 5000);

    expect(result).toEqual({
      type: "error",
      message: "HTTP timeout after 5000ms: https://api.example.com/data",
    });
  });

  it("handles 4xx/5xx status codes as success with status", async () => {
    globalThis.fetch = vi.fn().mockResolvedValue({
      status: 404,
      headers: new Headers([["content-type", "application/json"]]),
      json: () => Promise.resolve({ error: "Not found" }),
    } as unknown as Response);

    const result = await handleHttpFetch(baseEffect);

    expect(result).toEqual({
      type: "success",
      value: { status: 404, body: { error: "Not found" } },
    });
  });
});
