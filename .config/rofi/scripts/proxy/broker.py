import collections
from writer import Writer
from receiver import Receiver
from typing import Dict, List, Optional, Any
from utils import cancel_all_tasks, ExitException


class Broker:
    def __init__(self, loop, writer: Writer, receivers: List[Receiver]):
        self._loop = loop
        self._writer = writer
        self._waiter = None
        self._queue: collections.deque = collections.deque()
        self._exclusive_receiver: Optional[Receiver] = None
        self._receivers = {receiver.get_group(): receiver for receiver in receivers}
        self._mode_receivers = {receiver.get_activate_word(): receiver
                                for receiver in receivers
                                if receiver.get_activate_word() is not None}

    def add_message(self, msg: Dict[str, Any]):
        self._queue.append(msg)
        if self._waiter is not None and not self._waiter.done():
            self._waiter.set_result(None)

    async def _get_next_message(self) -> Dict[str, Any]:
        if not self._queue:
            waiter = self._loop.create_future()
            self._waiter = waiter
            await waiter
        return self._queue.popleft()

    def _view_next_message(self) -> Optional[Dict[str, Any]]:
        if not self._queue:
            return None
        return self._queue[0]

    def _send(self):
        self._writer.send(self._receivers.values())

    def _on_init(self):
        for receiver in self._receivers.values():
            receiver.on_init()
        self._send()

    def _on_activate(self, activated_receiver: Receiver):
        prompt = activated_receiver.get_prompt()
        if prompt is not None:
            self._writer.set_prompt(prompt)
        self._writer.set_input("")
        self._writer.exit_by_cancel(False)
        self._writer.hide_combi_lines(True)

        activated_receiver.set_activated(True)
        activated_receiver.on_input("")
        for receiver in self._receivers.values():
            if receiver != activated_receiver:
                receiver.set_activated(False)

        self._exclusive_receiver = activated_receiver
        self._send()

    def _on_deactivate(self):
        self._writer.set_prompt("proxy")
        self._writer.set_input("")
        self._writer.exit_by_cancel(True)
        self._writer.hide_combi_lines(False)

        self._exclusive_receiver.set_activated(False)
        for receiver in self._receivers.values():
            if receiver.get_activate_word() is None:
                receiver.set_activated(True)
                receiver.on_input("")

        self._exclusive_receiver = None
        self._send()

    def _on_input(self, text: str):
        if self._exclusive_receiver is None:
            activated_receiver: Optional[Receiver] = self._mode_receivers.get(text, None)
            if activated_receiver is not None and not activated_receiver.is_activated():
                self._on_activate(activated_receiver)
                return

        next_msg = self._view_next_message()
        if next_msg is not None and next_msg["name"] == "input":
            return

        for receiver in self._receivers.values():
            if receiver.is_activated():
                receiver.on_input(text)
        self._send()

    def _on_key_press(self, key: str, line: Dict[str, str]):
        if key == "cancel":
            if self._exclusive_receiver is not None:
                self._on_deactivate()
        else:
            receiver = self._receivers.get(line["group"], None)
            if receiver is not None:
                receiver.on_key_press(key, line["id"], line["text"])
                self._send()

    def _on_enter(self, line: Dict[str, str]):
        receiver = self._receivers.get(line["group"], None)
        if receiver is not None:
            receiver.on_enter(line["id"], line["text"])
            self._send()

    async def _run(self):
        self._on_init()

        while True:
            msg = await self._get_next_message()
            name = msg["name"]
            if name == "input":
                self._on_input(msg["value"])
            elif name == "key_press":
                self._on_key_press(msg["value"]["key"], msg["value"]["line"])
            elif name == "select_line":
                self._on_enter(msg["value"])

    async def run(self):
        try:
            await self._run()
        except ExitException:
            await cancel_all_tasks()
