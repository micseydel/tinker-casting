import os
import sys
import logging

from slack_bolt import App
from slack_bolt.adapter.socket_mode import SocketModeHandler
from slack_sdk.web.client import WebClient

from woke_notes import WokeNote, get_config_from_env


def slack_handler(bot_token, app_token, actor):
    # https://docs.slack.dev/tools/bolt-python/concepts/socket-mode/
    app = App(token=bot_token)
    actor.tell(app.client)

    @app.message()
    def handle_message(say, message):
        print(f"RECEIVED {type(message)} {message}")
        say(f"Hello from your local bot! You said: {message['text']}")
        actor.tell(message)

    @app.event("app_mention")
    def handle_app_mention_events(body, say, message):
        print(f"BODY {type(body)} {body}")
        say(f"You mentioned {message}")

    return SocketModeHandler(app, app_token)


class SlackTinkering(WokeNote):
    slack_client: WebClient | None = None

    def on_start(self):
        super().on_start()
        if not self.my_note.already_exists():
            self.my_note.set_file_contents("""---
to_channel: "(fill this in)"
markdown_text: "*This is not through say()*"
---
- [ ] send message
""")

    def on_note_modified(self):
        maybe_note = self.my_note.note_if_markdown_starts_with_pressed_button()
        if maybe_note:
            frontmatter, markdown = maybe_note

            to_channel = frontmatter.get("to_channel")
            markdown_text = frontmatter.get("markdown_text")

            if to_channel is not None and markdown_text is not None:
                self.slack_client.chat_postMessage(channel=to_channel, markdown_text=markdown_text)
            else:
                logging.warning("need both to_channel and markdown_text in frontmatter")

            self.my_note.reset_button_at_start_of_markdown()
        else:
            logging.warning(f"No note [[{self.my_note.note_name}]]")

    def on_other_message(self, message):
        if isinstance(message, WebClient):
            self.slack_client = message
        else:
            logging.exception(f"weird message {message}")


def main():
    bot_token = os.environ.get("SLACK_BOT_TOKEN")
    app_token = os.environ.get("SLACK_APP_TOKEN")

    if not app_token or not bot_token:
        raise ValueError("Missing SLACK_APP_TOKEN or SLACK_BOT_TOKEN in environment variables.")

    try:
        _, vault_path = sys.argv
    except ValueError:
        print("Expected vault_path CLI param\n", file=sys.stderr)
        exit(1)

    mqtt_config = get_config_from_env()
    WokeNote.start_background_actors(vault_path, mqtt_config)
    actor = SlackTinkering.wake("Slack Tinkering")

    handler = slack_handler(bot_token, app_token, actor)
    try:
        handler.start()
    except KeyboardInterrupt:
        print("\nExiting per Ctrl+C")
    finally:
        actor.stop()
        WokeNote.stop_background_actors()


if __name__ == "__main__":
    main()





