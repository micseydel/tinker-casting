import logging
import sys
import asyncio
from typing import Tuple

import pykka
from claude_agent_sdk import (
    ClaudeSDKClient, ClaudeAgentOptions,
    AssistantMessage, TextBlock, ResultMessage,
)
from claude_agent_sdk.types import PermissionResultAllow, PermissionResultDeny, ToolPermissionContext, SystemMessage, UserMessage

from woke_notes import get_config_from_env, WokeNote


class ToolUseRequest:
    future: pykka.ThreadingFuture

    def __init__(self, future, tool_name: str, input_data, context: ToolPermissionContext):
        self.tool_name = tool_name
        self.input_data = input_data
        self.context = context
        self.future = future


class ClaudeCodeHarnessNote(WokeNote):
    tool_use_request: None | ToolUseRequest = None

    def on_start(self):
        super().on_start()

    def on_note_modified(self):
        if (tool_use_request := self.tool_use_request) is not None:
            # check for answers
            frontmatter, markdown = self.my_note.get_note()
            if not markdown:
                logging.warning(f"Markdown was empty, ignoring")
                return
            yes, no = markdown.splitlines()[-2:]

            logging.info(f"Checking for tool use answer for {tool_use_request.tool_name} with input date {tool_use_request.input_data} and context {tool_use_request.context}")

            if yes.startswith("- [x] Yes"):
                tool_use_request.future.set(PermissionResultAllow(updated_input=tool_use_request.input_data))
                self.tool_use_request = None
            elif no.startswith("- [x] No"):
                tool_use_request.future.set(PermissionResultDeny(message="denied by user"))
                self.tool_use_request = None
            else:
                logging.info("The note was modified during an ongoing tool use request, but no decision was detected")

    def on_other_message(self, message):
        if isinstance(message, AssistantMessage):
            result_lines = ["# Result (?)", ""]
            for block in message.content:
                if isinstance(block, TextBlock):
                    # logging.info(f"block.text: {block.text}")
                    result_lines.append(block.text)
                else:
                    logging.info(f"Expected a TextBlock but got {type(block)}: {block}")

            if len(result_lines) == 2:
                logging.info(f"Got a TextBlock, but no blocks: {message}")
                return

            result_lines.append("")
            self.my_note.append("\n".join(result_lines))
        elif isinstance(message, UserMessage):
            logging.info(f"user message (e.g. blocked tool use): {message}")
        elif isinstance(message, ResultMessage):
            logging.info(f"\n[done] message.subtype {message.subtype}")
        elif isinstance(message, SystemMessage):
            logging.info(f"system message subtype {message.subtype}, data: {message.data}")
        elif isinstance(message, ToolUseRequest):
            self.tool_use_request = message
            # FIXME: if input_data is json, I could use json.dumps(, indent=4)
            self.my_note.append(f"""## Tool use request ({message.tool_name})

Input data: `{message.input_data}`

Context:
```
{message.context}
```

Decision Point
- [ ] Yes
- [ ] No
""")
        else:
            logging.info(f"unexpected message: {message}")

    # proxy functions

    def get_prompt(self) -> Tuple[str, str]:
        """
        Returns (model, prompt)
        """
        frontmatter, markdown = self.my_note.get_note()

        prompt = markdown # markdown.split("# Prompt\n", 2)[1].lstrip()
        model = frontmatter.get("model", "claude-haiku-4-5")

        return model, prompt


async def ask_permission(actor, tool_name: str, input_data, context: ToolPermissionContext):
    fut = pykka.ThreadingFuture()
    actor.tell(ToolUseRequest(fut, tool_name, input_data, context))
    return await fut


async def main():
    try:
        _, vault_path = sys.argv
    except ValueError:
        print("need vault_path on cli", file=sys.stderr)
        exit(1)

    mqtt_config = get_config_from_env()
    WokeNote.start_background_actors(vault_path, mqtt_config)

    try:
        harness = ClaudeCodeHarnessNote.wake("ClaudeCodeHarness")
        proxy = harness.proxy()

        model, prompt = proxy.get_prompt().get()  # FIXME rename
        logging.info(f"Using model {model}")

        # FIXME: can include a system_prompt
        options = ClaudeAgentOptions(
            model=model,

            # model="claude-haiku-4-5",
            # model="claude-sonnet-5",
            # model="claude-opus-5",

            can_use_tool=lambda tool_name, input_data, context: ask_permission(harness, tool_name, input_data, context),
        )

        async with ClaudeSDKClient(options=options) as client:
            await client.query(prompt)
            async for message in client.receive_response():
                harness.tell(message)

        logging.info("Running done")
    except KeyboardInterrupt:
        print("Exiting per Ctrl+C")
    finally:
        WokeNote.stop_background_actors()

if __name__ == "__main__":
    asyncio.run(main())
