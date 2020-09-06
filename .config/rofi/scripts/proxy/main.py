#!/bin/python

import asyncio
from writer import Writer
from reader import Reader
from broker import Broker


async def main():
    loop = asyncio.get_event_loop()
    writer = Writer()
    broker = Broker(writer)
    reader = Reader(loop, broker)
    await asyncio.gather(broker.run(), reader.run())


if __name__ == "__main__":
    asyncio.run(main())
