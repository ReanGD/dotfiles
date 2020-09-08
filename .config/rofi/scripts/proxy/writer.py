import sys
import json
from typing import List
from receiver import Receiver


class Writer:
    def __init__(self):
        self._help = None
        self._input = None

    def set_help(self, text: str):
        self._help = text

    def set_input(self, text: str):
        self._input = text

    def send(self, receivers: List[Receiver]):
        lines = []
        for receiver in receivers:
            lines.extend(receiver.get_lines())

        req = {"lines": lines}
        if self._help is not None:
            req["help"] = self._help
            self._help = None
        if self._input is not None:
            req["input"] = self._input
            self._input = None

        sys.stdout.write(json.dumps(req) + "\n")
        sys.stdout.flush()
