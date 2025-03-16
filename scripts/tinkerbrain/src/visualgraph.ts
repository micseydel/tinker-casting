import {isTinkerNode, TinkerEdge, TinkerGraph, TinkerNode} from './graph.ts'
import * as d3 from "d3";
import {BaseType} from "d3";

//
const width: number = 1200;
const height: number = 800;

export class VisualGraph {
    graph: TinkerGraph;
    frames: string[][] = [];

    private d3EdgeSelection: d3.Selection<SVGLineElement, TinkerEdge, SVGGElement, unknown>;
    private d3NodeSelection: d3.Selection<SVGCircleElement, TinkerNode, SVGGElement, unknown>;
    private currentFrameDisplay: d3.Selection<HTMLDivElement, unknown, HTMLElement, undefined>;
    private slider: d3.Selection<BaseType, unknown, HTMLElement, any>;

    private isPlaying: boolean = false;
    private playButton: d3.Selection<BaseType, unknown, HTMLElement, any>;

    constructor(container: HTMLElement, nodes: TinkerNode[], edges: TinkerEdge[], existingFrames: TinkerEdge[][]) {
        this.graph = new TinkerGraph(nodes, edges);

        this.frames = existingFrames.map(edges => edges.map(te => (te.source) + "->" + (te.target)))

        this.playButton = d3.select(container)
            .append('button')
            .text('Play Animation')
            .on('click', () => this.togglePlayAnimation());

        // d3.select(container)
        //     .append('button')
        //     .text('Download All SVGs')
        //     .on('click', async (event) => {
        //         const button = event.target as HTMLButtonElement;
        //         button.disabled = true;
        //         button.textContent = 'Downloading...';
        //
        //         try {
        //             await this.downloadAllSvgs();
        //             button.textContent = 'Download All SVGs';
        //         } catch (error) {
        //             console.error('Error downloading SVGs:', error);
        //             button.textContent = 'Download All SVGs (Error occurred)';
        //         } finally {
        //             button.disabled = false;
        //         }
        //     });

        const svg: d3.Selection<SVGSVGElement, unknown, null, any> = d3.select(container).append("svg")
            .style("border", "2px solid black")
            .attr("width", width)
            .attr("height", height);

        const edgeGroup: d3.Selection<SVGGElement, unknown, null, any> = svg.append("g");
        const nodeGroup: d3.Selection<SVGGElement, unknown, null, any> = svg.append("g");

        const pinSomeNodes = false
        if (pinSomeNodes) {
            nodes.forEach(node => {

                if (node.id === "AkkaActor/system/TinkerCast/Orchestrator/Gossiper") {
                    node.fx = width / 2;
                    node.fy = height / 2;
                }
                if (node.id === "AkkaActor/system/TinkerCast/Orchestrator/Applications/CatsHelper") {
                    node.fx = width / 2 - 400; // left of the Gossiper
                    node.fy = height / 2;
                }

                if (node.id === "AkkaActor/system/TinkerCast/Orchestrator/Applications/CatsHelper/CatTranscriptionListener") {
                    node.fx = width / 2 - 400;
                    node.fy = height / 2 + 50;
                }

                if (node.id === "AkkaActor/system/TinkerCast/Orchestrator/Applications/CentralNervousSystemMaintenance/Halto") {
                    node.fx = width / 2 + 200; // right
                    node.fy = height / 2 - 200; // and above
                }

                if (node.id === "AkkaActor/system/TinkerCast/Orchestrator/HueControl") {
                    node.fx = width / 2 - 200; // left
                    node.fy = height / 2 - 200; // and above
                }

                // if (node.id === "AkkaActor/system/TinkerCast/Orchestrator/Applications/CentralNervousSystemMaintenance/Halto") {  // Replace with the specific node's ID or condition
                //     node.fx = 200;
                //     node.fy = 150;
                // } else if (node.id === "AkkaActor/system/TinkerCast/Orchestrator/Gossiper") {
                //     node.fx = 1000;
                //     node.fy = 200;
                // }
            });

        }

        this.d3NodeSelection = nodeGroup
            .selectAll(".node")
            .data(nodes)
            .enter()
            .append(d => d.href ? document.createElementNS("http://www.w3.org/2000/svg", "a") : document.createElementNS("http://www.w3.org/2000/svg", "g"))
            .attr("class", "node")
            .each(function(d) {
                if (d.href) {
                    // d3.select(this).attr("xlink:href", d.href)
                    //     .attr("target", "_blank")
                    // ;
                    d3.select(this)
                        // .on("click", function(event, d) {
                        //     event.preventDefault();
                        //     let popup = window.open(d.href, 'newwindow', 'width=100, height=100');
                        //     setTimeout(() => { popup.close(); }, 500); // Adjust timing as needed
                        // });
                        .attr("xlink:href", d.href)
                        .style("cursor", "pointer")
                        .on("mouseover", function() { d3.select(this).style("fill", "darkblue"); })
                        .on("mouseout", function() { d3.select(this).style("fill", "blue"); });
                }
            })
            .append("circle")
            .attr("r", 10)
            .style("fill", d => d.color);

        this.d3EdgeSelection = edgeGroup
            .selectAll(".edge")
            .data(edges)
            .enter().append("line")
            .attr("class", "edge")
            .style("stroke-width", 1.5)
            .style("stroke-opacity", 0.2)
        ;

        // Append text to the same parent element (either <a> or <g>)
        const emojiText = this.d3NodeSelection
            .select(function() { return this.parentNode; })
            .append("text")
            .attr("class", "node-emoji")
            .attr("dx", d => d.x ?? 0)
            .attr("dy", d => (d.y ?? 0) + 5)
            .attr("text-anchor", "middle")
            .attr("alignment-baseline", "central")
            .style("font-size", "10px")
            .text(tinkerNode => tinkerNode.emoji);

        const tooltip = d3.select("#tooltip");


        this.d3NodeSelection.on("mouseover", function (event: MouseEvent, d: TinkerNode) {
            tooltip.transition()
                .duration(200)
                .style("opacity", .9);
            tooltip.html(`${uriToActorAndSupervisor(d.id)}`)
                .style("left", (event.pageX + 10) + "px")
                .style("top", (event.pageY - 28) + "px");

            d3.select(this)
                .style("stroke", "yellow")
                .attr("r", 12);
        })
            .on("mouseout", function(d) {
                tooltip.transition()
                    .duration(500)
                    .style("opacity", 0);

                d3.select(this)
                    .style("stroke", null)
                    .attr("r", 10); // reset to original radius
            });


        // @ts-ignore unused variables
        this.d3NodeSelection.on("mouseout", function (event: MouseEvent, d: TinkerNode) {
            d3.select(this)
                .style("stroke", "#fff")
                .attr("r", 10);
        });

        this.d3EdgeSelection.append("title")
            .text(d => getLastPathSegment(d.source) + "->" + getLastPathSegment(d.target));

        // @ts-ignore unused variables
        this.d3EdgeSelection.on("mouseover", function (event: MouseEvent, d: TinkerEdge) {
            d3.select(this)
                .style("stroke", "pink");
        });

        // @ts-ignore unused variables
        this.d3EdgeSelection.on("mouseout", function (event: MouseEvent, d: TinkerEdge) {
            d3.select(this)
                .style("stroke", "#000");
        });

        const simulation: d3.Simulation<TinkerNode, undefined> = d3.forceSimulation(nodes)
            .force("link", d3.forceLink<TinkerNode, d3.SimulationLinkDatum<TinkerNode>>(edges).id(d => d.id))
            .force("charge", d3.forceManyBody().strength(-50))
            .force("center", d3.forceCenter(width / 2, height / 2))
            .force("collision", d3.forceCollide().radius(20));


        simulation.on("tick", () => {
            nodes.forEach(boundedBox());

            this.d3EdgeSelection
                .attr("x1", d => isTinkerNode(d.source) ? d.source.x : 0)
                .attr("y1", d => isTinkerNode(d.source) ? d.source.y : 0)
                .attr("x2", d => isTinkerNode(d.target) ? d.target.x : 0)
                .attr("y2", d => isTinkerNode(d.target) ? d.target.y : 0);

            this.d3NodeSelection
                .attr("cx", d => d.x!)
                .attr("cy", d => d.y!);

            emojiText // Update the position of the emoji text
                .attr("x", d => d.x!)
                .attr("y", d => d.y!);
        });

        simulation.force<d3.ForceLink<TinkerNode, TinkerEdge>>("link")!
            .links(edges);

        this.slider = d3.select("#frame-slider").attr("max", this.frames.length.toString());
        this.currentFrameDisplay = d3.select("#current-frame");

        this.slider.on("input", () => {
            const input = d3.select("#frame-slider").node() as HTMLInputElement;
            const frame: number = +input.value;
            this.goToFrame(frame);
            const newUrl = `${window.location.pathname}?frame=${frame}`;
            window.history.pushState({}, "", newUrl);
        });

        const urlParams = new URLSearchParams(window.location.search);
        const urlSpecifiedFrame = urlParams.get('frame');
        if (urlSpecifiedFrame) {
            // relies on this.slider
            const specifiedUrlFrame = parseInt(urlSpecifiedFrame, 10);
            console.log(`specifiedUrlFrame ${specifiedUrlFrame}`)
            this.goToFrame(specifiedUrlFrame);
        } else {
            const lastFrame = this.frames.length;
            console.log(`lastFrame ${lastFrame}`)
            this.goToFrame(lastFrame);
        }

        edges.forEach(edge => {
            edge.source = nodes.find(n => n.id === edge.source) || edge.source;
            edge.target = nodes.find(n => n.id === edge.target) || edge.target;
        });
    }

    // external functions

    addFrame(highlightedEdges: string[]): void {
        this.frames.push(highlightedEdges);
        console.log(`adding frame, total frames now ${this.frames.length}`);
        this.selectEdges(highlightedEdges);
        this.currentFrameDisplay.text(this.frames.length.toString());
        this.slider
            .attr("max", this.frames.length.toString())
            .attr("value", this.frames.length)
        ;
    }

    goToFrame(frame: number): void {
        const safeFrame = Math.min(frame, this.frames.length);
        const safeIndex = safeFrame - 1;
        console.log(`selecting index (frame-1) ${safeIndex} where request was for (${frame}) from total of ${this.frames.length} frames`);
        this.selectEdges(this.frames[safeIndex]);
        this.slider.attr("value", safeFrame);
        const newUrl = `${window.location.pathname}?frame=${safeFrame}`;
        window.history.pushState({}, "", newUrl);
        this.currentFrameDisplay.text(safeFrame.toString());
    }

    private selectEdges(highlightedEdges: string[]): void {
        this.d3EdgeSelection
            .style("stroke", d => colorForEdge(d, highlightedEdges))
            .style("stroke-width", d => doesInclude(d, highlightedEdges) ? 5 : 1.5)
            .style("stroke-opacity", d => doesInclude(d, highlightedEdges) ? 1 : 0.2)
        ;
    }


    private togglePlayAnimation(): void {
        if (this.isPlaying) {
            this.stopAnimation();
        } else {
            this.playAnimation();
        }
    }

    private async playAnimation(): Promise<void> {
        this.isPlaying = true;
        this.playButton.text('Stop Animation');

        for (let i = 1; i <= this.frames.length; i++) {
            if (!this.isPlaying) break;
            this.goToFrame(i);
            await new Promise(resolve => setTimeout(resolve, 500)); // Adjust delay as needed
        }

        this.isPlaying = false;
        this.playButton.text('Play Animation');
    }

    private stopAnimation(): void {
        this.isPlaying = false;
        this.playButton.text('Play Animation');
    }



    private async downloadAllSvgs(): Promise<void> {
        for (let i = 1; i <= this.frames.length; i++) {
            this.goToFrame(i);
            await new Promise(resolve => setTimeout(resolve, 100)); // Wait for rendering
            await this.downloadSvg(`frame_${i.toString().padStart(4, '0')}.svg`);
            await new Promise(resolve => setTimeout(resolve, 100)); // Small delay between downloads
        }
    }

    private async downloadSvg(filename: string): Promise<void> {
        const svgElement = d3.select('svg').node() as SVGElement;

        const serializer = new XMLSerializer();
        let source = serializer.serializeToString(svgElement);

        if (!source.match(/^<svg[^>]+xmlns="http\:\/\/www\.w3\.org\/2000\/svg"/)) {
            source = source.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
        }
        if (!source.match(/^<svg[^>]+"http\:\/\/www\.w3\.org\/1999\/xlink"/)) {
            source = source.replace(/^<svg/, '<svg xmlns:xlink="http://www.w3.org/1999/xlink"');
        }

        source = '<?xml version="1.0" standalone="no"?>\r\n' + source;

        const url = "data:image/svg+xml;charset=utf-8," + encodeURIComponent(source);

        return new Promise<void>((resolve) => {
            const downloadLink = document.createElement("a");
            downloadLink.href = url;
            downloadLink.download = filename;
            document.body.appendChild(downloadLink);
            downloadLink.click();
            document.body.removeChild(downloadLink);
            resolve();
        });
    }
}

function stringEdge(source: string, target: string): string {
    return `${source}->${target}`;
}

function doesInclude(d: TinkerEdge, highlightedEdges: string[]): boolean {
    let sourceId = isTinkerNode(d.source) ? d.source.id : d.source;
    let targetId = isTinkerNode(d.target) ? d.target.id : d.target;
    const stringifiedEdge: string = stringEdge(sourceId, targetId);
    return highlightedEdges.includes(stringifiedEdge);
}

function colorForEdge(d: TinkerEdge, highlightedEdges: string[]): string {
    const newHighlightColor = isTinkerNode(d.source) ? d.source.color : "red";
    return doesInclude(d, highlightedEdges) ? newHighlightColor : "#000";
}

export function getLastPathSegment(inputString: string): string {
    return inputString.split('/').pop()!;
}

export function uriToActorAndSupervisor(uri: string): string {
    const parts = uri.split('/');
    const actorName = parts.pop()!;
    const supervisor = parts.pop()!;
    return `${actorName} (under ${supervisor})`;
}

function boundedBox() {
    const minX = 10,
        minY = 10,
        maxX = width - 10,
        maxY = height - 10;

    return (node: TinkerNode) => {
        if (node.x! < minX) node.vx += (minX - node.x!) * 0.1; // Push right
        if (node.x! > maxX) node.vx += (maxX - node.x!) * 0.1; // Push left
        if (node.y! < minY) node.vy += (minY - node.y!) * 0.1; // Push down
        if (node.y! > maxY) node.vy += (maxY - node.y!) * 0.1; // Push up
    };
}
