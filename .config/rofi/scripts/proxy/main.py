#!/bin/python

import sys
import asyncio
from writer import Writer
from reader import Reader
from broker import Broker


async def main():
    module = "interactive"
    if len(sys.argv) >= 2:
        name = sys.argv[1].strip()
        if name in ["translate", "custom_menu"]:
            module = name

    loop = asyncio.get_event_loop()
    writer = Writer()
    broker = Broker(loop, writer, module)
    reader = Reader(loop, broker)
    try:
        await asyncio.gather(broker.run(), reader.run())
    except asyncio.exceptions.CancelledError:
        pass

if __name__ == "__main__":
    asyncio.run(main())