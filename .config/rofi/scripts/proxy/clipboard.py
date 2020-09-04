from typing import Union
from subprocess import Popen, PIPE


def get_clipboard() -> str:
    p = Popen(["xclip", "-out", "-selection", "clipboard"], stdout=PIPE, close_fds=True, encoding="utf-8")
    stdout, _ = p.communicate()
    return stdout


def get_primary_clipboard() -> str:
    p = Popen(["xclip", "-out", "-selection", "primary"], stdout=PIPE, close_fds=True, encoding="utf-8")
    stdout, _ = p.communicate()
    return stdout


def set_clipboard(value: Union[str, int, float, bool]):
    acceptedTypes = (str, int, float, bool)
    if not isinstance(value, acceptedTypes):
        cls_name = value.__class__.__name__
        raise Exception(f"only str, int, float, and bool values can be copied to the clipboard, not {cls_name}")

    p = Popen(["xclip", "-selection", "clipboard"], stdin=PIPE, close_fds=True, encoding="utf-8")
    p.communicate(input=str(value))
