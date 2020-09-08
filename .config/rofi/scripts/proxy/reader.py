import sys
import json
import asyncio
from broker import Broker


class Reader:
    def __init__(self, loop, broker: Broker):
        self._loop = loop
        self._broker = broker

    async def _get_stdin_reader(self):
        reader = asyncio.StreamReader(loop=self._loop)
        reader_protocol = asyncio.StreamReaderProtocol(reader)
        await self._loop.connect_read_pipe(lambda: reader_protocol, sys.stdin)
        return reader

    async def run(self):
        reader = await self._get_stdin_reader()

        while True:
            line = await reader.readline()
            input_msg = json.loads(line)
            self._broker.add_message(input_msg)
