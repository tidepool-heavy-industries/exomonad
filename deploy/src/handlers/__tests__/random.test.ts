/**
 * Tests for random effect handlers.
 */

import { describe, it, expect } from "vitest";
import { handleRandomInt } from "../random.js";
import type { RandomIntEffect } from "exomonad-generated-ts";

describe("handleRandomInt", () => {
  describe("range validation", () => {
    it("returns error when min > max", async () => {
      const effect: RandomIntEffect = {
        type: "RandomInt",
        eff_min: 10,
        eff_max: 5,
      };

      const result = await handleRandomInt(effect);

      expect(result).toEqual({
        type: "error",
        message: "RandomInt: min (10) > max (5)",
      });
    });

    it("returns the value when min === max", async () => {
      const effect: RandomIntEffect = {
        type: "RandomInt",
        eff_min: 7,
        eff_max: 7,
      };

      const result = await handleRandomInt(effect);

      expect(result).toEqual({
        type: "success",
        value: 7,
      });
    });
  });

  describe("valid ranges", () => {
    it("returns value within range for standard dice (1-6)", async () => {
      const effect: RandomIntEffect = {
        type: "RandomInt",
        eff_min: 1,
        eff_max: 6,
      };

      // Run multiple times to increase confidence
      for (let i = 0; i < 100; i++) {
        const result = await handleRandomInt(effect);
        expect(result.type).toBe("success");
        if (result.type === "success") {
          expect(result.value).toBeGreaterThanOrEqual(1);
          expect(result.value).toBeLessThanOrEqual(6);
          expect(Number.isInteger(result.value)).toBe(true);
        }
      }
    });

    it("returns value within range for 0-based range", async () => {
      const effect: RandomIntEffect = {
        type: "RandomInt",
        eff_min: 0,
        eff_max: 10,
      };

      for (let i = 0; i < 50; i++) {
        const result = await handleRandomInt(effect);
        expect(result.type).toBe("success");
        if (result.type === "success") {
          expect(result.value).toBeGreaterThanOrEqual(0);
          expect(result.value).toBeLessThanOrEqual(10);
        }
      }
    });

    it("handles negative ranges", async () => {
      const effect: RandomIntEffect = {
        type: "RandomInt",
        eff_min: -10,
        eff_max: -5,
      };

      for (let i = 0; i < 50; i++) {
        const result = await handleRandomInt(effect);
        expect(result.type).toBe("success");
        if (result.type === "success") {
          expect(result.value).toBeGreaterThanOrEqual(-10);
          expect(result.value).toBeLessThanOrEqual(-5);
        }
      }
    });

    it("handles ranges spanning zero", async () => {
      const effect: RandomIntEffect = {
        type: "RandomInt",
        eff_min: -5,
        eff_max: 5,
      };

      for (let i = 0; i < 50; i++) {
        const result = await handleRandomInt(effect);
        expect(result.type).toBe("success");
        if (result.type === "success") {
          expect(result.value).toBeGreaterThanOrEqual(-5);
          expect(result.value).toBeLessThanOrEqual(5);
        }
      }
    });

    it("handles large ranges", async () => {
      const effect: RandomIntEffect = {
        type: "RandomInt",
        eff_min: 0,
        eff_max: 1000000,
      };

      for (let i = 0; i < 20; i++) {
        const result = await handleRandomInt(effect);
        expect(result.type).toBe("success");
        if (result.type === "success") {
          expect(result.value).toBeGreaterThanOrEqual(0);
          expect(result.value).toBeLessThanOrEqual(1000000);
        }
      }
    });
  });

  describe("distribution (smoke test)", () => {
    it("generates variety of values over many iterations", async () => {
      const effect: RandomIntEffect = {
        type: "RandomInt",
        eff_min: 1,
        eff_max: 6,
      };

      const seen = new Set<number>();
      for (let i = 0; i < 100; i++) {
        const result = await handleRandomInt(effect);
        if (result.type === "success") {
          seen.add(result.value as number);
        }
      }

      // After 100 rolls, we should have seen most/all values 1-6
      // This isn't a rigorous statistical test, just a smoke test
      expect(seen.size).toBeGreaterThanOrEqual(4);
    });
  });
});
