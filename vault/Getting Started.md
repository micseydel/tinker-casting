- Idea - TaskNotes for setup?
	- Homebrew installation
	- sbt (once)
	- ffmpeg (once)

# FYI

This is intended to be viewed with [Obsidian](https://obsidian.md/) although Obsidian isn't FOSS and isn't strictly required. If you're on modern Mac then following the instructions should be sufficient to get things working, but I'm not sure about Windows or the exact instructions for Linux. (see: [[Why Obsidian]])

# Getting Started (on Mac)

- [ ] Install [Home Brew](https://brew.sh) for dependency management
- [ ] Install sbt
	- `brew install sbt`
- Open a terminal in `tinker-casting`
	- [ ] Run `sbt compile`
	- *If it doesn't work*, you probably need the right Java version
		- Install [sdkman](https://sdkman.io/) (once)
			- (If you already have a system in place for managing multiple Java versions, you can consider using that instead of installing SDK man)
		- `sdk install java 17.0.0-tem` (once)
		- `source ~/.sdkman/bin/sdkman-init.sh`
			- You may see **java.lang.NoClassDefFoundError: Could not initialize class sbt.internal.parser.SbtParser$** or similar in the future (e.g. [[What happens with Java versions beyond 17]]) then run the "source" command again
	- Configure the environment
		- [ ] `cp tinkerenv.bash.template tinkerenv.bash`
		- [ ] **Modify** your new tinkerenv.bash file's line like this...
			- `export vaultRoot="/Users/TinkerCaster/vaults/tinker-starter-vault"`
			- ...so that the path within the quotes points to to this vault, the directory containing this file
	- Run:
		- `./tinkerenv.bash`

# Demo options

- [[Voice Transcription]]
	- Reminders
	- [[Hue Smart Lights]]
	- Cat litter tracking
		- Notification center
		- Summary report, audit, chart
			- [[Charts Plugin]]
- Recurring Responsibilities
	- Voice completion
- Notification center
	- *enhancements*
		- [[Chime Sounds]]
		- [[Hue Smart Lights]]
		- [[Ntfy for mobile Push Notifications]] (requires internet)
	- as an Obsidian pane
		- with a custom icon
- [[Tinkerbrain visualization]]
	- [ ] *helping with debugging*
- Air quality
	- [[PurpleAirCloudAPI]] (requires API key)
	- PurpleAir local (if you have the hardware for it)
	- AirGradient local
	- Aranet4 local Bluetooth
- *Google*
	- Gmail
	- Google Calendar
- Barely-integrated AI
	- Koroko voice synthesis
	- Ollama
		- *who no OpenAI integration*
- Wyze smart plugs
- ([[Python setup]])
- (no longer maintained)
	- Fitbit
- ~
	- ==[[Recommended Tinkering]]== (open this note in a new tab and then come back)

# Future plans

- Rasa flow/loop
- "Projects" as a more coherent idea

# Technicals

- [[Viewing the source code]]
- Logging
	- logs/???.log *examples*
		- application (*everything*)
		- error-warn
		- notifications
		- halto
		- foodreminder
		- cats
		- rememberingtimekeeper
	- Note - debug contains A LOT more info
	- [[Errors and warning considered normal for this demo]] (non-exhaustive)
- [[Errors and warning considered normal for this demo]]
- [ ] "Make sure to `source py3.10_venv/bin/activate` ==[[#^cd6662|every time]]=="
- [ ] Selecting the JDK may be required - [[What happens with Java versions beyond 17]]
