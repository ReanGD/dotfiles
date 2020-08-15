import psutil
from message import Message


class Kill:
    def __init__(self):
        self.lines = {}

    def add_line(self, user_text: str, value: str, msg: Message):
        msg.add_line(user_text)
        self.lines[user_text] = value

    def on_input(self, user_text: str, msg: Message) -> bool:
        self.lines = {}
        if user_text != "kill ":
            return False

        for it in psutil.process_iter(["pid", "name"]):
            self.add_line(f"{it.info['pid']} {it.info['name']}", it.info['pid'], msg)

        return True

    def on_enter(self, user_text: str) -> bool:
        value = self.lines.get(user_text, None)
        if value is None:
            return False

        return True
