#!/bin/python

import sys
import asyncio
from icon import Icon
from writer import Writer
from reader import Reader
from broker import Broker
from storage import Storage


async def main():
    module = "interactive"
    if len(sys.argv) >= 2:
        name = sys.argv[1].strip()
        if name in ["translate", "custom_menu"]:
            module = name

    loop = asyncio.get_event_loop()
    storage = Storage(writer=Writer(), icon_resolver=Icon())
    broker = Broker(loop, storage, module)
    reader = Reader(loop, broker)
    try:
        await asyncio.gather(broker.run(), reader.run())
    except asyncio.exceptions.CancelledError:
        pass

if __name__ == "__main__":
    asyncio.run(main())
