from __future__ import annotations
from typing import List, Any, Optional, TYPE_CHECKING
if TYPE_CHECKING:
    from icon import Icon
    from writer import Writer
    from storage import Storage


class Receiver:
    def __init__(self, storage: Storage, group: str, activate_word: str = None, prompt: str = None):
        self._group = group
        self._lines: List[Any] = []
        if activate_word is None:
            self._activated = True
            self._activate_word = None
        else:
            self._activated = False
            self._activate_word = activate_word.strip() + " "
        self._prompt = prompt
        self.writer: Writer = storage.writer
        self._icon_resolver: Icon = storage.icon_resolver

    def get_group(self) -> str:
        return self._group

    def is_activated(self) -> bool:
        return self._activated

    def set_activated(self, value: bool):
        self._activated = value

    def get_activate_word(self) -> Optional[str]:
        return self._activate_word

    def get_prompt(self) -> Optional[str]:
        return self._prompt

    def get_lines(self) -> List[Any]:
        return self._lines

    def reset_lines(self):
        self._lines = []

    def resolve_icon(self, name: str) -> str:
        return self._icon_resolver.resolve(name)

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

    def on_key_press(self, key: str, id_text: str, text: str):
        pass

    def on_enter(self, id_text: str, text: str):
        pass
