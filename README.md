**FYI** This is a **"learning in public" personal code base**. Instructions for getting started can be found [here](https://github.com/micseydel/tinker-starter-vault/). This is not polished and I tried to embrace my imperfectionism and -- at times -- a little bit of chaos. It definitely has bugs, use at your own risk.

# About

## Applying my Tinker Cast

- Single-stream [voice transcriptions](documentation/Applications%20of%20my%20Tinker%20Cast%20-%20voice%20transcriptions.md)
- ⭐️Voice-based [cat litter tracking](documentation/Applications%20of%20my%20Tinker%20Cast%20-%20cat%20litter%20tracking.md) ⭐️
- [Smart light](documentation/Applications%20of%20my%20Tinker%20Cast%20-%20smart%20lights%20control.md) controls using a note (or voice)
- ⭐️A text-centered [notification center](documentation/Applications%20of%20my%20Tinker%20Cast%20-%20notification%20center.md) ⭐️
- Air quality monitoring
- HALT (hungry/angry/lonely/tired)

## Some Context

- I had recently left a hybrid backend and data engineering [role](https://techblog.livongo.com/etl-from-mongo-to-redshift/), where I worked with a [former Evernote CTO](https://hackernoon.com/livongos-cto-dave-engberg-on-servant-leadership-communicating-with-executive-peers-more-620a2a92b203)
- One of my two cats had recently developed a [life-threatening chronic condition](https://vcahospitals.com/know-your-pet/feline-idiopathic-cystitis)
- I had started a personal wiki of networked notes ("PKMS" = personal knowledge management system)

This project represents the intersection of those things. The name is a play on words, with "Cast" referring to the underlying actor model from computer science, as well as an allusion to casting magic spells.

# Some more details

So, I wanted to use my data engineering knowledge to automate the process of leveraging my personal wiki in order to help me take care of my cats. The nodes (circles) in the graphic below show my actors/atomic agents and the edges (lines) indicate their communication.

![](https://i.imgur.com/on94H7Y.png)
- ([older graphic](https://i.imgur.com/ErAay7m.png))
- [click here for an animation](https://imgur.com/a/extended-mind-visualization-2024-10-20-Hygmvkq)

Here, we see atomic agents dedicated to
- My cats 🐱 (in brown)
- My smart lights 🕹️💡
- A sleep report 😴 and my Fitbit ⌚️
- Tracking when I last ate and reminding me if needed 🫢🥗
- Enabling other agents to react to time passing ⏰
- A notification center ❗️
  - Notifications via [Ntfy](https://ntfy.sh/) 📧
- [HALT](https://health.clevelandclinic.org/halt-hungry-angry-lonely-tired) and CO2 tracking 🛑
- etc

## Technical details

- Tech used includes
  - Akka 2.6 (Scala) for the actor model
  - OpenAI Whisper for transcription
  - Rasa open source for entity extraction
- The actors/atomic agents are generally **explicit encodings** in Scala, rather than AI
- Most of it works fully offline
- The visualization is a hack, frontend is my biggest weakness

# Inspiration includes

- Knowledge management (links)
  - [A text renaissance](https://www.mentalnodes.com/a-text-renaissance) (yay for explicit encodings)
  - [Networked thinking](https://www.appsntips.com/what-is-networked-thinking/)
  - [Mind gardening](https://www.refinery29.com/en-us/2022/04/10953988/mind-gardening-organize-thoughts-increase-productivity)
- [The Extended Mind](https://anniemurphypaul.com/books/the-extended-mind/) (2021 book)
  - You can almost think of this as part 4, "Thinking With Code"
  - (...and AI if/when needed)
- [Kasparov's Law](https://news.northeastern.edu/2024/06/17/garry-kasparov-chess-humans-ai/)
  - > \[...] which states that a human of average intelligence and an AI system working together in harmony is more effective than either working alone, and even more advantageous than a brilliant human working with a system poorly
  - I personally don't see AI as integral to Kasparov's law, it applies to tech in general
  - As a specific example, my [cat litter tracking](documentation/Applications%20of%20my%20Tinker%20Cast%20-%20cat%20litter%20tracking.md) was initially supposed to answer the question, **"When did Peanut last pee?"** but I built the summary report along the way, and that (combined with infrequent manual investigations) has probably been a better trade-off for now 

# Related ideas

- "Cognitive light cones" by [Michael Levin](https://www.youtube.com/watch?v=WLHO39qvcO4)
  - You can learn more on [Youtube](https://www.youtube.com/watch?v=YnObwxJZpZc) or through [his paper](https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2019.02688/full)
  - Related to the [similarly-named idea from physics](https://en.wikipedia.org/wiki/Light_cone), our cognitive light cones include the potential inputs to our current moment
  - My Tinker Cast helps provide the right past information at the right moment to inform future decisions
- The [Thousand Brains hypothesis](https://www.numenta.com/resources/books/a-thousand-brains-by-jeff-hawkins/)
  - You can think of the actors as **externalized reference frames**
  - However many cortical columns were dedicated to worrying about before, I got them back; it's kind of like having **expanded my cortical column count** by adding _specialized_ virtual ones
  - I would say that I'm "**Building a Thousand Brains**" more than "Building a Second Brain" (a productivity brand)
  - I need to follow [Monty's](https://github.com/thousandbrainsproject/tbp.monty) example and add functionality for **anticipation/suprises and voting**

It can be thought of a civilian version of the "hyper-enabled operator"
- [AI-Powered Super Soldiers Are More Than Just a Pipe Dream](https://www.wired.com/story/us-military-hyper-enabled-operator/) (_Wired_, may be paywalled)
- [The Hyper-Enabled Operator](https://web.archive.org/web/20241103233351/https://smallwarsjournal.com/jrnl/art/hyper-enabled-operator) (Small Wars Journal linked to by _Wired_ above, archived link)
  - > USSOCOM defines HEO as _a SOF professional empowered by technologies that enhance the operator’s cognition at the edge by increasing situational awareness, reducing cognitive load, and accelerating decision making._  In short, HEO hyper-enables the operator by providing technological aids to shorten the time within his OODA loop, thereby providing him with cognitive overmatch.
  - I have my situational awareness around: my cats, my air quality, and HALT-related things
  - I have less cognitive load, more peace of mind
  - I can make faster decisions because data has already been gathered and digested
  - If applied to video games (say, Age of Empires or DOTA2) I might achieve "**cognitive overmatch**"

# See also

Similar projects
- [Atomic Agents](https://github.com/BrainBlend-AI/atomic-agents) (Python, available on pypi)
- [Jido](https://elixirforum.com/t/jido-a-sdk-for-building-autonomous-agent-systems/68418/5) (Elixir)
