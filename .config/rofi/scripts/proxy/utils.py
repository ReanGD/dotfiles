import os
import asyncio


class ExitException(Exception):
    pass


async def cancel_all_tasks():
    tasks = [t for t in asyncio.all_tasks() if t is not asyncio.current_task()]
    for task in tasks:
        task.cancel()


def get_icon(name: str) -> str:
    cfg_path = os.environ.get("XDG_CONFIG_HOME", None)
    if cfg_path is None:
        cfg_path = os.path.join(os.environ["HOME"], ".config")

    return os.path.join(cfg_path, "rofi", "icons", name)
