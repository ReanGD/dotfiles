import asyncio
from typing import Dict
from writer import Writer
from translate import Translate
from utils import cancel_all_tasks, ExitException


class Broker:
    def __init__(self, writer: Writer):
        self.__writer = writer
        self.__queue: asyncio.Queue = asyncio.Queue()
        receivers = [Translate(writer)]
        self.__receivers = {receiver.get_group(): receiver for receiver in receivers}

    def add_message(self, msg: Dict[str, object]):
        self.__queue.put_nowait(msg)

    def __send(self):
        self.__writer.send(self.__receivers.values())

    def __on_init(self):
        for receiver in self.__receivers.values():
            receiver.on_init()
        self.__send()

    def __on_input(self, text: str):
        for receiver in self.__receivers.values():
            receiver.on_input(text)
        self.__send()

    def __on_enter(self, line: Dict[str, str]):
        receiver = self.__receivers.get(line["group"], None)
        if receiver is not None:
            receiver.on_enter(line["id"], line["text"])
            self.__send()

    async def _run(self):
        self.__on_init()

        while True:
            msg = await self.__queue.get()
            name = msg["name"]
            if name == "input":
                self.__on_input(msg["value"])
            elif name == "select_line":
                self.__on_enter(msg["value"])

            self.__queue.task_done()

    async def run(self):
        try:
            await self._run()
        except ExitException:
            await cancel_all_tasks()
