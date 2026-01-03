export type GraphId = "example" | "test";
export interface GraphEdge {
    from: string;
    to: string;
}
export interface GraphInfo {
    id: GraphId;
    name: string;
    nodes: string[];
    edges: GraphEdge[];
}
export declare const graphRegistry: Record<GraphId, GraphInfo>;
//# sourceMappingURL=graphs.d.ts.map