import sys
import json
from typing import List
from receiver import Receiver


class Writer:
    def __init__(self):
        self.__help = None
        self.__input = None

    def set_help(self, text: str):
        self.__help = text

    def set_input(self, text: str):
        self.__input = text

    def send(self, receivers: List[Receiver]):
        lines = []
        for receiver in receivers:
            lines.extend(receiver.get_lines())

        req = {"lines": lines}
        if self.__help is not None:
            req["help"] = self.__help
            self.__help = None
        if self.__input is not None:
            req["input"] = self.__input
            self.__input = None

        sys.stdout.write(json.dumps(req) + "\n")
        sys.stdout.flush()
