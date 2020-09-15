#!/bin/python

import sys
import asyncio
from icon import Icon
from calc import Calc
from kill import Kill
from typing import List
from writer import Writer
from reader import Reader
from broker import Broker
from storage import Storage
from receiver import Receiver
from translate import Translate
from custom_menu import CustomMenu


async def main():
    module = "interactive"
    if len(sys.argv) >= 2:
        name = sys.argv[1].strip()
        if name in ["translate", "custom_menu"]:
            module = name

    loop = asyncio.get_event_loop()
    writer = Writer()
    storage = Storage(writer=writer, icon_resolver=Icon())
    receivers: List[Receiver] = []
    if module == "translate":
        receivers = [Translate(storage)]
    elif module == "custom_menu":
        receivers = [CustomMenu(storage)]
    else:
        receivers = [Calc(storage), Kill(storage)]

    broker = Broker(loop, writer, receivers)
    reader = Reader(loop, broker)
    try:
        await asyncio.gather(broker.run(), reader.run())
    except asyncio.exceptions.CancelledError:
        pass

if __name__ == "__main__":
    asyncio.run(main())
