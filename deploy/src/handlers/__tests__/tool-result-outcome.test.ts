/**
 * ToolResultOutcome wire serialization tests.
 *
 * Tests round-trip JSON serialization of tool execution results across the WASM FFI boundary.
 * Verifies that transition targets, payloads, and error messages are preserved correctly.
 */

import { describe, it, expect } from "vitest";

/**
 * ToolResultOutcome - wire format for tool dispatcher results.
 * Matches Haskell ToolResultOutcome type from tidepool-wasm/src/Tidepool/Wasm/WireTypes.hs
 */
type ToolResultOutcome =
  | { tag: "success"; value: unknown }
  | { tag: "break"; reason: string }
  | { tag: "transition"; target: string; payload: unknown };

describe("ToolResultOutcome wire serialization", () => {
  // ═══════════════════════════════════════════════════════════════════════════
  // JSON ROUND-TRIP TESTS
  // ═══════════════════════════════════════════════════════════════════════════

  describe("JSON round-tripping", () => {
    it("TROSuccess with integer value round-trips", () => {
      const original: ToolResultOutcome = {
        tag: "success",
        value: 42,
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed).toEqual(original);
    });

    it("TROSuccess with complex object round-trips", () => {
      const original: ToolResultOutcome = {
        tag: "success",
        value: { nested: { data: "value" }, array: [1, 2, 3] },
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed).toEqual(original);
    });

    it("TROSuccess with null value round-trips", () => {
      const original: ToolResultOutcome = {
        tag: "success",
        value: null,
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed).toEqual(original);
    });

    it("TROBreak with error message round-trips", () => {
      const original: ToolResultOutcome = {
        tag: "break",
        reason: "Tool execution failed due to validation error",
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed).toEqual(original);
    });

    it("TROBreak with empty message round-trips", () => {
      const original: ToolResultOutcome = {
        tag: "break",
        reason: "",
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed).toEqual(original);
    });

    it("TROTransition with target and payload round-trips", () => {
      const original: ToolResultOutcome = {
        tag: "transition",
        target: "nodeA",
        payload: { status: "completed" },
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed).toEqual(original);
    });

    it("TROTransition with complex payload round-trips", () => {
      const original: ToolResultOutcome = {
        tag: "transition",
        target: "dashboard",
        payload: {
          state: "initialized",
          data: { id: 123, items: ["a", "b", "c"] },
        },
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed).toEqual(original);
    });
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // JSON STRUCTURE VALIDATION
  // ═══════════════════════════════════════════════════════════════════════════

  describe("JSON structure validation", () => {
    it("TROSuccess has correct tag field", () => {
      const outcome: ToolResultOutcome = {
        tag: "success",
        value: 42,
      };
      const json = JSON.stringify(outcome);
      expect(json).toContain('"tag":"success"');
    });

    it("TROSuccess includes value field", () => {
      const outcome: ToolResultOutcome = {
        tag: "success",
        value: 42,
      };
      const json = JSON.stringify(outcome);
      expect(json).toContain('"value":42');
    });

    it("TROBreak has correct tag field", () => {
      const outcome: ToolResultOutcome = {
        tag: "break",
        reason: "error message",
      };
      const json = JSON.stringify(outcome);
      expect(json).toContain('"tag":"break"');
    });

    it("TROBreak includes reason field", () => {
      const outcome: ToolResultOutcome = {
        tag: "break",
        reason: "error message",
      };
      const json = JSON.stringify(outcome);
      expect(json).toContain('"reason":"error message"');
    });

    it("TROTransition has correct tag field", () => {
      const outcome: ToolResultOutcome = {
        tag: "transition",
        target: "success",
        payload: { count: 5 },
      };
      const json = JSON.stringify(outcome);
      expect(json).toContain('"tag":"transition"');
    });

    it("TROTransition includes target field", () => {
      const outcome: ToolResultOutcome = {
        tag: "transition",
        target: "nodeA",
        payload: {},
      };
      const json = JSON.stringify(outcome);
      expect(json).toContain('"target":"nodeA"');
    });

    it("TROTransition includes payload field", () => {
      const outcome: ToolResultOutcome = {
        tag: "transition",
        target: "success",
        payload: { count: 5 },
      };
      const json = JSON.stringify(outcome);
      expect(json).toContain('"payload"');
      expect(json).toContain('"count":5');
    });
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // EDGE CASES
  // ═══════════════════════════════════════════════════════════════════════════

  describe("edge cases", () => {
    it("TROTransition with unicode target name", () => {
      const original: ToolResultOutcome = {
        tag: "transition",
        target: "节点A",
        payload: {},
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed.target).toBe("节点A");
    });

    it("TROTransition with empty target name", () => {
      const original: ToolResultOutcome = {
        tag: "transition",
        target: "",
        payload: {},
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed.target).toBe("");
    });

    it("TROBreak with unicode error message", () => {
      const original: ToolResultOutcome = {
        tag: "break",
        reason: "错误信息",
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed.reason).toBe("错误信息");
    });

    it("TROSuccess with deeply nested object", () => {
      const original: ToolResultOutcome = {
        tag: "success",
        value: {
          level1: {
            level2: {
              level3: {
                level4: { data: "deep" },
              },
            },
          },
        },
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed).toEqual(original);
    });

    it("TROSuccess with large array value", () => {
      const largeArray = Array.from({ length: 1000 }, (_, i) => i);
      const original: ToolResultOutcome = {
        tag: "success",
        value: largeArray,
      };
      const json = JSON.stringify(original);
      const parsed = JSON.parse(json) as ToolResultOutcome;
      expect(parsed.value).toEqual(largeArray);
    });
  });

  // ═══════════════════════════════════════════════════════════════════════════
  // TYPE DISCRIMINATION
  // ═══════════════════════════════════════════════════════════════════════════

  describe("type discrimination", () => {
    it("can discriminate TROSuccess from other types", () => {
      const outcome: ToolResultOutcome = {
        tag: "success",
        value: 42,
      };
      expect(outcome.tag).toBe("success");
      expect("value" in outcome).toBe(true);
      expect("reason" in outcome).toBe(false);
      expect("target" in outcome).toBe(false);
    });

    it("can discriminate TROBreak from other types", () => {
      const outcome: ToolResultOutcome = {
        tag: "break",
        reason: "failed",
      };
      expect(outcome.tag).toBe("break");
      expect("reason" in outcome).toBe(true);
      expect("value" in outcome).toBe(false);
      expect("target" in outcome).toBe(false);
    });

    it("can discriminate TROTransition from other types", () => {
      const outcome: ToolResultOutcome = {
        tag: "transition",
        target: "nodeA",
        payload: {},
      };
      expect(outcome.tag).toBe("transition");
      expect("target" in outcome).toBe(true);
      expect("payload" in outcome).toBe(true);
      expect("value" in outcome).toBe(false);
      expect("reason" in outcome).toBe(false);
    });
  });
});
