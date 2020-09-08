import collections
from calc import Calc
from writer import Writer
from receiver import Receiver
from translate import Translate
from custom_menu import CustomMenu
from typing import Dict, List, Optional, Any
from utils import cancel_all_tasks, ExitException


class Broker:
    def __init__(self, loop, writer: Writer, module: str):
        self._loop = loop
        self._writer = writer
        self._waiter = None
        self._queue: collections.deque = collections.deque()
        receivers: List[Receiver] = []
        if module == "translate":
            receivers = [Translate(writer)]
        if module == "custom_menu":
            receivers = [CustomMenu(writer)]
        else:
            receivers = [Calc(writer)]

        self._receivers = {receiver.get_group(): receiver for receiver in receivers}

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

    def _on_input(self, text: str):
        for receiver in self._receivers.values():
            receiver.on_input(text)
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
                next_msg = self._view_next_message()
                if next_msg is None or next_msg["name"] != "input":
                    self._on_input(msg["value"])
            elif name == "select_line":
                self._on_enter(msg["value"])

    async def run(self):
        try:
            await self._run()
        except ExitException:
            await cancel_all_tasks()
