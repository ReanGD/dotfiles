from __future__ import annotations


class Message:
    def __init__(self):
        self.lines = []
        self.header = None
        self.prompt = None
        self.is_send_action = None

    def set_action(self, is_send: bool) -> Message:
        self.is_send_action = is_send
        return self

    def set_prompt(self, text: bool) -> Message:
        self.prompt = text
        return self

    def add_line(self, msg: str) -> Message:
        self.lines.append(msg)
        return self

    def add_markup_line(self, msg: str) -> Message:
        self.lines.append({"text": msg, "markup": True})
        return self

    def build(self):
        msg = {}
        if self.header is not None:
            msg["message"] = self.header

        if len(self.lines) != 0:
            msg["lines"] = self.lines

        if self.prompt is not None:
            msg["prompt"] = self.prompt

        if self.is_send_action is not None:
            msg["input action"] = "send" if self.is_send_action else "filter"

        return msg
