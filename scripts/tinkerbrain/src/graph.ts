import * as d3 from "d3";

export interface TinkerNode extends d3.SimulationNodeDatum {
    id: string;
    color: string;
    emoji?: string;
    x?: number;
    y?: number;
    href?: string;
}

export interface TinkerEdge {
    source: string | TinkerNode;
    target: string | TinkerNode;
}

export function isTinkerNode(node: any): node is TinkerNode {
    return node && typeof node === 'object' && 'id' in node;
}

export class TinkerGraph {
    nodes: TinkerNode[];
    edges: TinkerEdge[];

    constructor(nodes: TinkerNode[], edges: TinkerEdge[]) {
        this.nodes = nodes;
        this.edges = edges;
    }
}
