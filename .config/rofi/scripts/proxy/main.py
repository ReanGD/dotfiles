#!/bin/python

import asyncio
from writer import Writer
from reader import Reader
from broker import Broker


async def main():
    loop = asyncio.get_event_loop()
    writer = Writer()
    broker = Broker(loop, writer)
    reader = Reader(loop, broker)
    try:
        await asyncio.gather(broker.run(), reader.run())
    except asyncio.exceptions.CancelledError:
        pass

if __name__ == "__main__":
    asyncio.run(main())
