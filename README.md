**FYI** This is a **"learning in public" personal code base**. Instructions for getting started can be found [here](https://github.com/micseydel/tinker-starter-vault/). This is not polished and I tried to embrace my imperfectionism and -- at times -- a little bit of chaos.

# About

When I started this project, there was recent context:
- I had left a hybrid backend and data engineering [role](https://techblog.livongo.com/etl-from-mongo-to-redshift/)
- One of my two cats had recently developed a [life-threatening chronic condition](https://vcahospitals.com/know-your-pet/feline-idiopathic-cystitis)
- I had started a personal wiki of networked notes

So, I wanted to use my data engineering knowledge to automate the process of leveraging my personal wiki in order to help me take care of my cats. Inspired in part by ChatGPT, I wanted my notes to be able to talk to each other like a social network of humans:

![](https://i.imgur.com/ErAay7m.png)
([click here for an animation](https://imgur.com/a/extended-mind-visualization-2024-10-20-Hygmvkq))

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

- This project uses the Actor Model for async communication between responsibilities
  - The name is wordplay around this
  - Also "casting" could apply to spells/magic
- Tech used includes
  - Akka 2.6 (Scala)
  - OpenAI Whisper
  - Rasa open source

# See also

Inspiration includes
- [The Extended Mind](https://anniemurphypaul.com/books/the-extended-mind/)
  - You can almost think of this as part 4, "Thinking With Code"
- The [Thousand Brains hypothesis](https://www.numenta.com/resources/books/a-thousand-brains-by-jeff-hawkins/)
  - You can think of the actors as externalized reference frames

It can be thought of a civilian version of the "hyper-enabled operator"
- https://www.wired.com/story/us-military-hyper-enabled-operator/ (paywalled)
- https://web.archive.org/web/20241103233351/https://smallwarsjournal.com/jrnl/art/hyper-enabled-operator (deleted after the election)

Similar projects
- [Atomic Agents](https://github.com/BrainBlend-AI/atomic-agents) (Python, available on pypi)
- [Jido](https://elixirforum.com/t/jido-a-sdk-for-building-autonomous-agent-systems/68418/5) (Elixir)
