from __future__ import annotations
from typing import List, TYPE_CHECKING
if TYPE_CHECKING:
    from writer import Writer


class Receiver:
    def __init__(self, writer: Writer, group: str):
        self.__group = group
        self.__lines: List[object] = []
        self._writer = writer

    def get_group(self) -> str:
        return self.__group

    def get_lines(self) -> List[object]:
        return self.__lines

    def reset_lines(self):
        self.__lines = []

    def add_line(self, text: str, id_text: str = "", markup: bool = False, filtering: bool = True, icon: str = None):
        # pylint: disable=R0913
        self.__lines.append({
            "id": id_text,
            "text": text,
            "group": self.__group,
            "icon": icon,
            "filtering": filtering,
            "markup": markup})

    def on_init(self):
        pass

    def on_input(self, text: str):
        pass

    def on_enter(self, id_text: str, text: str):
        pass
