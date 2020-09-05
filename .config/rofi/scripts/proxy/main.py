#!/bin/python

import sys
import json
import asyncio
from typing import Dict
from writer import Writer
from translate import Translate


class Proxy:
    def __init__(self, loop):
        self.__loop = loop
        self.__writer = Writer()
        receivers = [Translate(self.__writer)]
        self.__receivers = {receiver.get_group(): receiver for receiver in receivers}

    async def _get_stdin_reader(self):
        reader = asyncio.StreamReader(loop=self.__loop)
        reader_protocol = asyncio.StreamReaderProtocol(reader)
        await self.__loop.connect_read_pipe(lambda: reader_protocol, sys.stdin)
        return reader

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

    async def run(self):
        reader = await self._get_stdin_reader()
        self.__on_init()

        while True:
            line = await reader.readline()
            input_msg = json.loads(line)
            name = input_msg["name"]
            if name == "input":
                self.__on_input(input_msg["value"])
            elif name == "select_line":
                self.__on_enter(input_msg["value"])


def main():
    loop = asyncio.get_event_loop()
    task = None
    try:
        proxy = Proxy(loop)
        task = proxy.run()
        loop.run_until_complete(task)
    except KeyboardInterrupt:
        task.close()
    finally:
        loop.close()


if __name__ == "__main__":
    main()
