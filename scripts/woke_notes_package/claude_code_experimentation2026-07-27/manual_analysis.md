- Next time I will prompt it with any questions, and to ask permission before exploring the code base
- What to do with silly stuff like this?

```python
# NOTE: no `if TYPE_CHECKING: from ..dsl import *` guard here on purpose.
# The script is exec'd with a scope that already provides every global it uses
# (my_note, logging, datetime, today, ...), and that guard's body would be an
# uncovered line under plain coverage.py. See Next Notes Mockup.claudecode.md.
```

- ...on the one hand it only came from it reading things into the context it should not have, so I may just focus on that for now
- again, same reasoning: it burnt tokens to make an inert on_start and explain why (though it could and should have been omitted)
- after adding the DSL import back, there was one issue so I fixed the hacky DSL types thing
- at this point I'm gonna run the tests with coverage after a quick read-through...
---
- I need to instruct it to not engage in monkey patching
- removed code until tests passed, 100% coverage
- checkpointing, then will cleanup code and consider how I want to do other-file creation before some manual testing
---
- ok, so what to do about the created file? no Python precedent here, but Scala...
- Scala has spawn()/cast(), so it's not the same...
- I think a need a way to spawn/cast/wake WokeNotes that isn't global...
- 
