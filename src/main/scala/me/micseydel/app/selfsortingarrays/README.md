This is my own take on [Classical Sorting Algorithms as a Model of Morphogenesis: self-sorting arrays reveal unexpected competencies in a minimal model of basal intelligence](https://arxiv.org/pdf/2401.05375) ([github](https://github.com/Zhangtaining/cell_research/)) using the **actor model (Akka)** and my wrapper for it, enabling easy integration with [Obsidian](https://obsidian.md). I do not have a complete mixed-agent implementation (and [couldn't find theirs](https://github.com/Zhangtaining/cell_research/issues/1)) but have tinkered with two algorithms:

**Bubble sort:**

https://github.com/user-attachments/assets/8b5dc4a2-12c3-4308-b391-42ad525086ae

**Insertion sort:**

https://github.com/user-attachments/assets/202fbb73-1b54-4fac-919b-380f611a712d

- How to run
  - Install sbt (e.g. `brew install sbt`)
  - Install [Obsidian](https://obsidian.md/download) (for pretty visualizations)
    - Create an Obsidian vault (a new folder unless you want to do otherwise)
  - set `vaultRoot` environment variable to the Obsidian vault path
  - Run with sbt:
    - `sbt 'runMain me.micseydel.app.selfsortingarrays.SelfSortingArrays'`
  - Open [[Self Sorting Arrays Probe]] in Obsidian, that is the interface

---

Algotypes
- bubble sort (working)
- insertion sort (working)
  - (less mature, e.g. no decision point history, Thread.sleep)
- selection sort (stub)
- merge sort (stub)

Various notes
- Each cell has a respective Obsidian note containing:
  - ...wikilinks to neighbors (allowing Obsidian to visualize the network) e.g. [Cell 0](exampleoutput/bubblesort/Cell%200%20(28).md)
  - ...a history of decisions by that cell e.g. [Cell 0 History](exampleoutput/bubblesort/Cell%200%20History.md)
- [[SelfSortingArrayDebugger]] contains a sequence diagram of inter-cell communication
- [[Self Sorting Lists.canvas]] Ordered Obsidian Canvas of the cells
  - better than the graph view for manual tinkering

Future Work
- Finish the stub algotypes
- Documentation for the pure-Akka version
- (checkpoint) Create a uniform cell interface for mixed-algotype testing
- Expand the probe mechanism
- Expand algotypes e.g. insertion sort going the opposite direction
- Experiment with frozen cells, random failures, and such
- Non-code algotypes, e.g. LLM "agents" or humans mixed in
