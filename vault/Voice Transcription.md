- Test by using the same capture script as earlier,
	- `python rec_unlimited_play.py ~/Tinker\ Casting\ Starter\ Kit/Tinker\ Casting\ Starter\ Vault/attachments/mobile_audio_captures/`
	- Look at the Python server output to see the transcription status
		- (Expect transcription time to be ~50-100% of the recording time)
	- Once the callback completes, check [[Transcribed mobile notes (2024-MM-DD)]], e.g. [[Transcribed mobile notes (2024-09-26)]] (Cmd+mouse over the note with the date corrected, it shouldn't take more than a moment to appear)
## Setup: Voice transcription

- `brew install ffmpeg`
- `cd scripts/whisper_wrapper` (within tinker-casting)
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

## Setup: machine-learning for voice transcription

- ==TODO: VERIFY==
	- ![[Pasted image 20241128135148.png]]
- [[#^cd6662|Within the venv]], navigate to `scripts` and train the model
	- `cd scripts`
	- [ ] `rasa train`
- For training data, see `nlu.yml`
- POTENTIALLY NEEDED
	- `brew install librdkafka`  ([ref](https://github.com/confluentinc/confluent-kafka-python/issues/995))
- ~
- Start the transcription and Rasa server
	- `python transcriber.py large 5001 ~/Tinker\ Casting\ Starter\ Kit/Tinker\ Casting\ Starter\ Vault/`
	- There's a lot of output, if everything goes ok you can ignore anything before this:
		- [[Pasted image 20241128135148.png]]
		- (your model name will vary)