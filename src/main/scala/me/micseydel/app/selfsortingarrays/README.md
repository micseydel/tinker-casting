![](https://i.imgur.com/yLhBwUg.gif)

This is a more "agentic" reimplementation of https://github.com/Zhangtaining/cell_research/ from [Classical Sorting Algorithms as a Model of Morphogenesis: self-sorting arrays reveal unexpected competencies in a minimal model of basal intelligence](https://arxiv.org/pdf/2401.05375)
- note that I could not find the chimeric part of the code https://github.com/Zhangtaining/cell_research/issues/1

- How to run
  - Install sbt (e.g. `brew install sbt`)
  - Install [Obsidian](https://obsidian.md/download) (for pretty visualizations)
    - Create an Obsidian vault (open a potentially new folder in the app)
  - environment variable for Obsidian
    - set `vaultRoot` enviornment variable to the Obsidian vault path
    - (pure Akka alternate does not require this)
  - Run with sbt:
    - `sbt 'runMain me.micseydel.app.selfsortingarrays.SelfSortingArrays'`
  - Open [[Self Sorting Arrays Probe]] in Obsidian, it has buttons

---

This implementation is different:
- it uses the actor model (Akka) rather than raw threads
- it's in Scala rather than Python
- the user interface is via Obsidian (through my Akka wrapper)

Algotypes
- bubble sort (working)
- insertion sort (working)
  - (less mature, e.g. no decision point history)
- selection sort (stub)
- merge sort (stub)

Limitations
- there is currently a centralized clock tick mechanism for bubble sort
- insertion sort needs to be updated to have closer to parity (cleaner tags, time delays via messages instead of Thread.sleep, clock tick config)
- the message protocols do not yet support chimeric testing

Other Comparisons
- probably more deterministic - single threaded (by config), cells are not fighting for a lock
- stronger bias toward local rather than global
  - e.g. bubble sort has a correction mechanism when swap race conditions are noticed

Other Features
- Each cell has a respective Obsidian note containing:
  - ...wikilinks to neighbors (allowing Obsidian to visualize the network)
  - ...a history of decisions by that cell
- Sequence diagram of inter-cell communication
- Ordered Obsidian Canvas of the cells

Future Work
- Finish the stub algotypes
- (checkpoint) Create a uniform cell interface for mixed-algotype testing
- Expand the probe mechanism
- Expand algotypes e.g. insertion sort going the opposite direction
- Experiment with frozen cells, random failures, and such
- Non-code algotypes, e.g. LLM "agents" or humans mixed in


MP4 @ https://imgur.com/a/4vwOQqK

# Screenshots

FIXME
