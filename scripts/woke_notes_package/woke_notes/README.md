Needs updates per recent refactoring and updates
---

This rudimentary module combines the *actor model* (1973) with *notes* that could for example be read and written with apps like Obsidian, along with mqtt for interprocess communication and file-watching so human note interactions can result in asynchronous alerts. This is an early draft that I am not quite yet recommending people run, but it's nearly there...

This results in something like literate programming (Knuth 1984) especially with hot reloading that lets you
- modify or trigger actor behavior by changing markdown
- modify actor behavior by modifying a hot-reloaded script
- modify actor behavior by adding dependencies and doing an app restart (e.g. after `pip install ...`)
- send messages between those different actors
- ...all quickly and easily with real-time feedback

This project is based on my "Tinker Cast" in Scala, a similar project with Akka 2.6 for the actor model instead of Pykka. That project combines many actors into a specialized agentic mesh that sits on top of my Obsidian vault as a personal digital assistant of sorts, an e pluribus unum approach that doesn't center AI (though I do use transcription and voice memos heavily). mqtt allows my Scala and Python "casts" to interact with each other (Kafka and similar would work).

TODO
- [ ] clean up a bit and publish to pypi
- [ ] test ruamel.yaml round trip updates (are comments preserved?)
- [ ] demo video of the scripts/

LIMITATIONS
- Pykka's thread/GIL limits are inherited, no multiprocessing support
  - FIXME
- globals are shared between hot-reloaded LiterateNotes (actors)
    - this is an attack vector if you run untrusted code

Like the rest of the code base, this is released under the MIT license. This module will be split out into a separate code base, but still retain the MIT license.

---

# Quickstart

- pip install from git
- OR- clone, install from local
- `python -m` to `example_scripts`
  - create scripts
  - create notes
  - (how much RAM do this use?)
- extend the class
  - 
- use multi-processing
  - 

# Inbox

- [[Woke Notes Orchestrator]] (contains)
  - [[ScriptedNotesOrchestrator]]
    - subscriber should use a LIST
    - ...button to refresh the subscribers?
  - [[LiterateNotesManager (EXPERIMENTAL)]] # FIXME
- limitations
    - if someone removes a function from a hot reloading script, they should replace it with `pass` in the body before removing it
    - circuit breakers
- future work
  - my_note to allow file-like behavior, e.g. print()s
  - way for thrown exceptions to be shown in the note more easily

