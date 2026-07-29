create a venv, if you'd like (recommended)

```
$ python3.10 -m venv venv_20261728
$ # . venv_20261728/bin/activate.fish # for fish shell
$ source venv_20261728/bin/activate # for fish BASH
```

pip install (local/dev mode at the time of this writing, although you could use a github link)

```
pip install --upgrade pip
pip install -r requirements.txt
```

Note that "woke_notes_package/src/woke_notes/example_scripts/Example - Daily Responsibility.py" was copied to `scripts/Recurring Reminder.py` but you can do so again if it falls out of date:
```
cp '../../woke_notes_package/src/woke_notes/example_scripts/Example - Daily Responsibility.py' "scripts/Recurring Reminder.py"
```

To run from this directory:
```
python -m woke_notes <full vault path> [--scripts `realpath scripts`]
```
