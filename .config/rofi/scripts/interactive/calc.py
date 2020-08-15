import math
import types
from message import Message
from clipboard import to_clipboard


class Calc:
    def __init__(self):
        self.globals_dict = {"__builtins__": {}}
        self.locals_dict = vars(math)
        self.lines = {}

    def add_line(self, user_text: str, value: str, msg: Message):
        msg.add_line(user_text)
        self.lines[user_text] = value

    def on_input(self, user_text: str, msg: Message) -> bool:
        self.lines = {}
        if user_text == "":
            return False

        try:
            processed_user_text = user_text.replace("^", "**")
            answer = eval(processed_user_text, self.globals_dict, self.locals_dict)
            if isinstance(answer, types.BuiltinFunctionType):
                return False

            self.add_line(f"{user_text} = {answer}", answer, msg)
            return True
        except Exception:
            return False

    def on_enter(self, user_text: str) -> bool:
        value = self.lines.get(user_text, None)
        if value is None:
            return False

        to_clipboard(value)
        return True
