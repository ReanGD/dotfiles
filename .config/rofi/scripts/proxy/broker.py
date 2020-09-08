import asyncio
from typing import Dict
from writer import Writer
from translate import Translate
from utils import cancel_all_tasks, ExitException


class Broker:
    def __init__(self, writer: Writer):
        self._writer = writer
        self._queue: asyncio.Queue = asyncio.Queue()
        receivers = [Translate(writer)]
        self._receivers = {receiver.get_group(): receiver for receiver in receivers}

    def add_message(self, msg: Dict[str, object]):
        self._queue.put_nowait(msg)

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
            msg = await self._queue.get()
            name = msg["name"]
            if name == "input":
                self._on_input(msg["value"])
            elif name == "select_line":
                self._on_enter(msg["value"])

            self._queue.task_done()

    async def run(self):
        try:
            await self._run()
        except ExitException:
            await cancel_all_tasks()
