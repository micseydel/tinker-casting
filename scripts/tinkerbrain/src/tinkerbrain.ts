import { VisualGraph } from './visualgraph.ts';
import { TinkerEdge, TinkerNode } from "./graph.ts";

export function setupBrain(container: HTMLElement) {
    const stateMachine: AppStateMachine = new AppStateMachine(container);
    const socket: WebSocket = new WebSocket('ws://localhost:5003/ws-messages');

    console.log("Setting socket.onmessage and socket.onerror");

    socket.onmessage = function (event) {
        stateMachine.processMessage(event.data);
    };

    socket.onerror = function (error: Event) {
        console.error('WebSocket Error: ', error);
    };
}

class AppStateMachine {
    private visualGraph: VisualGraph | undefined = undefined;
    private transcriptions: HTMLElement[] = [];

    constructor(private container: HTMLElement) {
        // Initially no transcription elements
    }

    public processMessage(data: string) {
        try {
            // console.log("data: " + data);
            const message = JSON.parse(data);
            switch (message.type) {
                case 'graph':
                    this.initializeGraph(message as GraphData);
                    break;
                case 'frame':
                    this.updateGraph(message as FrameData);
                    break;
                case 'transcription_frame':
                    this.handleTranscription(message as SendTranscriptionFrame);
                    break;
                case 'heartbeat':
                    break;
                default:
                    console.error(`Unknown message type received: ${message.type}`);
            }
        } catch (error) {
            console.error(`Failed to parse JSON: ${error}`);
            throw error;
        }
    }

    private initializeGraph(graphData: GraphData) {
        if (this.visualGraph === undefined) {
            this.visualGraph = new VisualGraph(this.container, graphData.nodes, graphData.edges, graphData.priorFrames);
            console.log('Graph initialized successfully');
        } else {
            console.error('Graph is already initialized');
        }
    }

    private updateGraph(frameData: FrameData) {
        if (this.visualGraph !== undefined) {
            this.visualGraph.addFrame(frameData.edges.map(edge => `${(edge.source)}->${(edge.target)}`));
        } else {
            console.error('Graph is not initialized yet');
        }
    }

    private truncateString(str: string): string {
        if (str.length <= 150) {
            return str;
        } else {
            return str.substring(0, 147) + '...';
        }
    }

    private handleTranscription(sendTranscriptionFrame: SendTranscriptionFrame) {
        console.log(`Received transcription: ${sendTranscriptionFrame.notedTranscription.capture.whisperResult.whisperResultContent.text}`);
        const transcriptionText = this.truncateString(sendTranscriptionFrame.notedTranscription.capture.whisperResult.whisperResultContent.text);
        this.addTranscription(this.prefix(sendTranscriptionFrame.notedTranscription.capture.whisperResult.whisperResultMetadata.model) + transcriptionText);
    }

    private prefix(model: string): string {
        if (model === "BaseModel") {
            return "⚡️ ";
        } else if (model === "LargeModel") {
            return "🐢 "
        } else {
            return "";
        }
    }

    private addTranscription(text: string) {
        const newTranscription = document.createElement('div');
        newTranscription.innerText = text;
        newTranscription.style.position = 'absolute';
        newTranscription.style.background = 'white';
        newTranscription.style.padding = '10px';
        newTranscription.style.border = '1px solid #ccc';
        newTranscription.style.borderRadius = '5px';
        newTranscription.style.maxWidth = '300px';
        newTranscription.style.left = '10px';
        newTranscription.style.whiteSpace = 'normal';

        const container = document.querySelector<HTMLElement>('#transcription-container');
        if (container) {
            container.appendChild(newTranscription);
        }

        this.transcriptions.unshift(newTranscription); // Add to the front of the list

        this.updateTranscriptionsPositions();
    }

    private updateTranscriptionsPositions() {
        let cumulativeHeight = 0;
        this.transcriptions.forEach((el, index) => {
            if (index > 0) {
                cumulativeHeight += this.transcriptions[index - 1].offsetHeight + 10; // Add space between elements
            }
            el.style.top = `${cumulativeHeight}px`; // Set top position based on the cumulative height of previous elements
        });
    }
}

interface GraphData {
    messageType: 'graph';
    nodes: TinkerNode[];
    edges: TinkerEdge[];
    priorFrames: TinkerEdge[][];
}

interface SendTranscriptionFrame {
    messageType: 'transcription_frame';
    edges: TinkerEdge[];
    notedTranscription: any; // FIXME
}

interface FrameData {
    messageType: 'frame';
    edges: TinkerEdge[];
}
