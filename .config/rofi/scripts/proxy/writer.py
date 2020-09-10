import sys
import json
from typing import List
from receiver import Receiver


class Writer:
    def __init__(self):
        self._help = None
        self._input = None
        self._prompt = None
        self._hide_combi_lines = None
        self._exit_by_cancel = None

    def set_help(self, text: str):
        self._help = text

    def set_input(self, text: str):
        self._input = text

    def set_prompt(self, text: str):
        self._prompt = text

    def hide_combi_lines(self, value: bool):
        self._hide_combi_lines = value

    def exit_by_cancel(self, value: bool):
        self._exit_by_cancel = value

    def send(self, receivers: List[Receiver]):
        lines = []
        for receiver in receivers:
            if receiver.is_activated():
                lines.extend(receiver.get_lines())

        req = {"lines": lines}
        if self._help is not None:
            req["help"] = self._help
            self._help = None
        if self._input is not None:
            req["input"] = self._input
            self._input = None
        if self._prompt is not None:
            req["prompt"] = self._prompt
            self._prompt = None
        if self._hide_combi_lines is not None:
            req["hide_combi_lines"] = self._hide_combi_lines
            self._hide_combi_lines = None
        if self._exit_by_cancel is not None:
            req["exit_by_cancel"] = self._exit_by_cancel
            self._exit_by_cancel = None

        sys.stdout.write(json.dumps(req) + "\n")
        sys.stdout.flush()
