This is a getting-started vault for [tinker-casting](https://github.com/micseydel/tinker-casting), an open source project that combines "atomic notes" (PKM) and the actor model (CS). It is intended to be viewed with [Obsidian](https://obsidian.md/) although Obsidian isn't FOSS and isn't strictly required.

- hello world
- playing with my cast
- enabling actors with note-config

---

2025-02-28 UPDATE !!! I'm making big changes right now, this vault isn't stable against the codebase. Hopefully it will be soon, with a stable branch for future reference, but feel free to reach out.


FYI - This is a "vault" intended to be viewed with [Obsidian](https://obsidian.md/) and manipulated with [this code repo](https://github.com/micseydel/tinker-casting). If you're viewing this [on Github](https://github.com/micseydel/tinker-starter-vault), you should download *both repos and and Obsidian* if you want to try it yourself. Obsidian isn't strictly required (and **isn't FOSS**) but it's how I interact with the folder of Markdown notes and is assumed everywhere but this paragraph.

> [!caution] FYI
> 
> If you run into problems, please create a Github issue
> - [tinker-starter-vault](https://github.com/micseydel/tinker-starter-vault/issues) for documentation or getting-started issues
> 	- ==these instructions need to be modified for Windows==, but likely work on Apple Silicon (where they were tested)
> - [tinker-casting](https://github.com/micseydel/tinker-casting/issues) for issues that are likely code rather than vault related
> 	- if unsure, default to the repository above

# This is Obsidian

> [!info] Clicking here will show the source code for this part of the note
> - Obsidian mostly uses [Markdown](https://en.wikipedia.org/wiki/Markdown)
> - This is a [Callout](https://help.obsidian.md/Editing+and+formatting/Callouts) - an Obsidian addition rather than pure Markdown

- Consider installing a "Community Plugin" for a code copy button
	- Open **Settings** with Cmd+P "open settings" or Cmd+,
	- Navigate to **Community plugins** on the left
	- "Turn on community plugins" after reading the blurb
	- Browse for "code copy" e.g. **Copy Inline Code** and *enable* after installing
	- (Obsidian bullets can be collapsed and expanded, e.g. by clicking to the left of "Consider installing..." above)
	- (Same for header sections like "[[#This is Obsidian]]")

# Getting Started

- Install Home Brew if you haven't already - [brew.sh](https://brew.sh)
- Install sbt
	- `brew install sbt`
- Open a terminal in `tinker-casting`
- Run `sbt compile`
- If it doesn't work, you probably need the right Java version
	- Install [sdkman](https://sdkman.io/) (once)
		- (If you already have a system in place for managing multiple Java versions, you can consider using that instead of installing SDK man)
	- `sdk install java 17.0.0-tem` (once)
	- `source ~/.sdkman/bin/sdkman-init.sh`
		- ==([[What happens with Java versions beyond 17|This may need to be run every time]])==

# Viewing the source

- **Notes:**
	- non-coders can return to this section later
	- if you skipped compilation above, IntelliJ will not work as well until an sbt compilation completes
- Setup [IntelliJ community edition](https://www.jetbrains.com/idea/download/?section=mac) (for example via [Jetbrains Toolbox](https://www.jetbrains.com/toolbox-app/))
	- Open `tinker-casting`
	- ("Trust Project" is [recommended](https://www.jetbrains.com/help/idea/2024.2/project-security.html?Project_security))
	- Open TinkerCasterApp.scala
	- Install Scala Plugin
		- I was prompted, but: Cmd+, for settings, search "plugins"
	- Setup Scala SDK (as prompted)
		- ![[Pasted image 20241123171341.png]]
		- Download 2.12.20
	- File > Project structure (or Cmd+;)
		- Set Java SDK to version 17
			- ![[Pasted image 20240922144608.png]]
			- *My first go-through, it was in "Detected SDKs"*
	- Load sbt Project
		- It's the hexagon near the top right of the window
		- ![[Pasted image 20241123172015.png]]
		- ![[Pasted image 20241123172022.png|240]]
		- Once expanded, use the button to the left of the plus (+) symbol to *Sync All sbt Projects*
		- ![[Pasted image 20241123172045.png|480]]
- PyCharm is better for the Python code, but the Python code gets fewer updates so 🤷

# Python setup

- `brew install ffmpeg`
- `cd scripts` (this is generally for everything in the Python section)
- `python3.10 -m venv py3.10_venv`
	- Python 3.10 is needed (note: ==the specific dependencies need better documentation==)
- (==May need to use Bash, not zsh, from here==)
	- (zsh may work, actually, but I'm not sure about fish shell)
- `source py3.10_venv/bin/activate` ==every new terminal session== ^cd6662
- `pip install --upgrade pip`
- `pip install -r requirements.txt`
	- (this may take some time to download things)
	- If it fails, try running it again
- Now let's confirm the install by downloading and running the transcription model, before getting the whole system up and going...
	- Do ***not*** "Click to create" below, Obsidian should detect the file being created and update automatically when the above command is used
	- Make a recording targeting this vault's `playground` folder, e.g.
		- `python tinker_demo.py ~/Tinker\ Casting\ Starter\ Kit/Tinker\ Casting\ Starter\ Vault/playground/`
		- The first time (only!), this will download the large model, which is ~3GB
		- Try to keep this window open while the transcription is happening; even after the download, a short transcription will still take a moment and you can run the command above repeatedly to see the list added to
	- ![[Playground]]
- Consider installing the IntelliJ Python Community Edition plugin if you're working much with the Python code itself

# Rasa

- **This cannot currently be skipped** - the transcription server fails if no trained model is detected
- [[#^cd6662|Within the venv]], navigate to `scripts` and train the model
	- `cd scripts`
	- [ ] `rasa train`
- For training data, see `nlu.yml`
- POTENTIALLY NEEDED
	- `brew install librdkafka`  ([ref](https://github.com/confluentinc/confluent-kafka-python/issues/995))

# ==Tinker Cast Deployment==

- Environment variables; in the repository root, not in scripts/
	- `cp tinkerenv.bash.template tinkerenv.bash`
	- Open tinkerenv.bash and modify it as-needed per the inline comments, e.g.
		- vaultRoot `/Users/tinkercaster/Tinker Casting Starter Kit/Tinker Casting Starter Vault`
		- audioWatchPath `/Users/tinkercaster/Tinker Casting Starter Kit/Tinker Casting Starter Vault/attachments/mobile_audio_captures`
		- Other things can probably be left as-is
	- `chmod +x tinkerenv.bash`
- In 3 separate terminals
	- `caffeinate ./tinkerenv.bash`
	- Within `scripts`, ensuring the venv is activated in both,
		- Start the transcription and Rasa server
			- `python transcriber.py large 5001 ~/Tinker\ Casting\ Starter\ Kit/Tinker\ Casting\ Starter\ Vault/`
			- There's a lot of output, if everything goes ok you can ignore anything before this:
				- ![[Pasted image 20241128135148.png]]
				- (your model name will vary)
		- Test by using the same capture script as earlier,
			- `python rec_unlimited_play.py ~/Tinker\ Casting\ Starter\ Kit/Tinker\ Casting\ Starter\ Vault/attachments/mobile_audio_captures/`
			- Look at the Python server output to see the transcription status
				- (Expect transcription time to be ~50-100% of the recording time)
			- Once the callback completes, check [[Transcribed mobile notes (2024-MM-DD)]], e.g. [[Transcribed mobile notes (2024-09-26)]] (Cmd+mouse over the note with the date corrected, it shouldn't take more than a moment to appear)
		- [ ] ==[[Recommended Tinkering]]== (open this note in a new tab and then come back)
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


# Tinkerbrain visualization (optional)

- Install npm if needed
	- `brew install npm`
- Navigate to `tinkerbrain`
	- `cd source_code/tinkerplot/tinkerbrain`
- `npm install`
- `npm run dev` (every time)

# After a full shutdown/restart

- Make sure to `source py3.10_venv/bin/activate` ==[[#^cd6662|every time]]==
- Selecting the JDK may be required - [[What happens with Java versions beyond 17]]
