export interface GraphWasmExports {
    initialize: (graphId: string, json: string) => string;
    step: (graphId: string, json: string) => string;
    getGraphInfo: (graphId: string) => string;
    getGraphState: (graphId: string) => string;
}
//# sourceMappingURL=exports.d.ts.map