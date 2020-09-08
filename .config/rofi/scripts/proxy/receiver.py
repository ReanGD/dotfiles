from __future__ import annotations
from typing import List, TYPE_CHECKING
if TYPE_CHECKING:
    from writer import Writer


class Receiver:
    def __init__(self, writer: Writer, group: str):
        self._group = group
        self._lines: List[object] = []
        self.writer = writer

    def get_group(self) -> str:
        return self._group

    def get_lines(self) -> List[object]:
        return self._lines

    def reset_lines(self):
        self._lines = []

    def add_line(self, text: str, id_text: str = "", markup: bool = False, filtering: bool = True, icon: str = None):
        # pylint: disable=R0913
        self._lines.append({
            "id": id_text,
            "text": text,
            "group": self._group,
            "icon": icon,
            "filtering": filtering,
            "markup": markup})

    def on_init(self):
        pass

    def on_input(self, text: str):
        pass

    def on_enter(self, id_text: str, text: str):
        pass
