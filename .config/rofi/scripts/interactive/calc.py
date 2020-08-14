import math
import types
from message import Message


class Calc:
    def __init__(self):
        self.globals_dict = {"__builtins__": {}}
        self.locals_dict = vars(math)

    def on_input(self, user_text: str, msg: Message) -> bool:
        if user_text == "":
            return False

        try:
            processed_user_text = user_text.replace("^", "**")
            answer = eval(processed_user_text, self.globals_dict, self.locals_dict)
            if isinstance(answer, types.BuiltinFunctionType):
                return False

            msg.add_line(f"{user_text} = {answer}")
            return True
        except Exception:
            return False
