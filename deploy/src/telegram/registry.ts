/**
 * Graph Registry and Topic Management Types
 *
 * Defines available graphs and their topic bindings for automatic
 * thread isolation via Telegram private chat topics (Bot API 9.3+).
 */

/**
 * Definition of a graph available in the system.
 */
export interface GraphDefinition {
	/** Unique identifier for this graph (used in routing) */
	id: string;
	/** Display name for the topic (shown in Telegram) */
	displayName: string;
	/** Brief description of what this graph does */
	description: string;
	/** Graph ID to pass to WASM when initializing */
	wasmGraphId: string;
}

/**
 * Registry of all available graphs.
 * Each graph will get its own topic created automatically.
 */
export const GRAPH_REGISTRY: GraphDefinition[] = [
	{
		id: "habitica",
		displayName: "📋 Habitica Tasks",
		description: "Voice memo task extraction and Habitica integration",
		wasmGraphId: "habitica",
	},
	{
		id: "dm",
		displayName: "🎲 DM Campaign",
		description: "Blades in the Dark dungeon master",
		wasmGraphId: "dm",
	},
	// Future: Add tidying, etc.
];

/**
 * Binding between a Telegram topic and a graph.
 * Stored in Durable Object storage for persistence.
 */
export interface TopicBinding {
	/** Telegram thread ID (from message_thread_id) */
	threadId: number;
	/** Graph ID this topic is bound to */
	graphId: string;
	/** Topic name (for display in admin console) */
	topicName: string;
	/** Whether this graph is still active (in GRAPH_REGISTRY) */
	active: boolean;
	/** When this binding was created */
	createdAt: number;
}
