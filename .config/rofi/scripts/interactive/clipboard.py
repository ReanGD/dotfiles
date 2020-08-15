import subprocess
from typing import Union


def to_clipboard(value: Union[str, int, float, bool]):
    acceptedTypes = (str, int, float, bool)
    if not isinstance(value, acceptedTypes):
        cls_name = value.__class__.__name__
        raise Exception(f"only str, int, float, and bool values can be copied to the clipboard, not {cls_name}")

    p = subprocess.Popen(["xclip", "-selection", "c"], stdin=subprocess.PIPE, close_fds=True)
    p.communicate(input=str(value).encode("utf-8"))
