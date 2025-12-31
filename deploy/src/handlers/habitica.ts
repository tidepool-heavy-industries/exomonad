/**
 * Habitica API effect handler.
 *
 * Handles EffHabitica effects from WASM, calling the Habitica API
 * and returning results in the expected format.
 */

import type { HabiticaEffect, EffectResult } from "../protocol.js";
import { successResult, errorResult } from "../protocol.js";

// Habitica API base URL
const HABITICA_API = "https://habitica.com/api/v3";

// Habitica API response types
interface HabiticaResponse<T> {
  success: boolean;
  data: T;
  message?: string;
}

interface HabiticaUser {
  _id: string;
  profile?: { name?: string };
  stats: { hp: number; mp: number; exp: number; gp: number };
}

interface HabiticaTask {
  _id: string;
  text: string;
  type: string;
  completed?: boolean;
  checklist?: Array<{ id: string; text: string; completed: boolean }>;
}

interface HabiticaScoreResult {
  delta: number;
  _tmp?: { drop?: { text: string } };
}

/**
 * Configuration for Habitica API authentication.
 * Credentials should be provided via environment variables.
 */
export interface HabiticaConfig {
  userId: string;
  apiToken: string;
}

/**
 * Handle a Habitica effect by calling the appropriate API endpoint.
 */
export async function handleHabitica(
  effect: HabiticaEffect,
  config: HabiticaConfig
): Promise<EffectResult> {
  const headers = {
    "Content-Type": "application/json",
    "x-api-user": config.userId,
    "x-api-key": config.apiToken,
    "x-client": "tidepool-wasm",
  };

  try {
    switch (effect.eff_hab_op) {
      case "GetUser": {
        const resp = await fetch(`${HABITICA_API}/user`, {
          method: "GET",
          headers,
        });
        const data = (await resp.json()) as HabiticaResponse<HabiticaUser>;
        if (!resp.ok) throw new Error(data.message || "GetUser failed");
        return successResult({
          userId: data.data._id,
          userName: data.data.profile?.name ?? "",
          userStats: {
            usHp: data.data.stats.hp,
            usMp: data.data.stats.mp,
            usExp: data.data.stats.exp,
            usGp: data.data.stats.gp,
          },
        });
      }

      case "ScoreTask": {
        const payload = effect.eff_hab_payload as {
          taskId: string;
          direction: string;
        };
        const resp = await fetch(
          `${HABITICA_API}/tasks/${payload.taskId}/score/${payload.direction.toLowerCase()}`,
          { method: "POST", headers }
        );
        const data = (await resp.json()) as HabiticaResponse<HabiticaScoreResult>;
        if (!resp.ok) throw new Error(data.message || "ScoreTask failed");
        return successResult({
          srDelta: data.data.delta,
          srDrop: data.data._tmp?.drop?.text ?? null,
        });
      }

      case "GetTasks": {
        const payload = effect.eff_hab_payload as { taskType: string };
        const typeParam = payload.taskType.toLowerCase();
        const resp = await fetch(
          `${HABITICA_API}/tasks/user?type=${typeParam}`,
          {
            method: "GET",
            headers,
          }
        );
        const data = (await resp.json()) as HabiticaResponse<HabiticaTask[]>;
        if (!resp.ok) throw new Error(data.message || "GetTasks failed");
        return successResult(
          data.data.map((t) => ({
            taskId: t._id,
            taskText: t.text,
            taskType: t.type,
            taskCompleted: t.completed ?? null,
          }))
        );
      }

      case "FetchTodos": {
        // Legacy operation - same as GetTasks("todos")
        const resp = await fetch(`${HABITICA_API}/tasks/user?type=todos`, {
          method: "GET",
          headers,
        });
        const data = (await resp.json()) as HabiticaResponse<HabiticaTask[]>;
        if (!resp.ok) throw new Error(data.message || "FetchTodos failed");
        return successResult(
          data.data.map((t) => ({
            todoId: t._id,
            todoTitle: t.text,
            todoChecklist:
              t.checklist?.map((c) => ({
                checklistId: c.id,
                checklistText: c.text,
                checklistDone: c.completed,
              })) ?? [],
            todoCompleted: t.completed,
          }))
        );
      }

      case "CreateTodo": {
        const payload = effect.eff_hab_payload as { title: string };
        const resp = await fetch(`${HABITICA_API}/tasks/user`, {
          method: "POST",
          headers,
          body: JSON.stringify({ text: payload.title, type: "todo" }),
        });
        const data = (await resp.json()) as HabiticaResponse<HabiticaTask>;
        if (!resp.ok) throw new Error(data.message || "CreateTodo failed");
        return successResult({ unTodoId: data.data._id });
      }

      case "AddChecklistItem": {
        const payload = effect.eff_hab_payload as {
          todoId: string;
          item: string;
        };
        const resp = await fetch(
          `${HABITICA_API}/tasks/${payload.todoId}/checklist`,
          {
            method: "POST",
            headers,
            body: JSON.stringify({ text: payload.item }),
          }
        );
        const data = (await resp.json()) as HabiticaResponse<HabiticaTask>;
        if (!resp.ok)
          throw new Error(data.message || "AddChecklistItem failed");
        // Return the new checklist item ID
        const checklist = data.data.checklist ?? [];
        const lastItem = checklist[checklist.length - 1];
        return successResult(lastItem.id);
      }

      default:
        return errorResult(`Unknown Habitica operation: ${effect.eff_hab_op}`);
    }
  } catch (err) {
    return errorResult(err instanceof Error ? err.message : String(err));
  }
}
