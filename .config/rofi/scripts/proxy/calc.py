import math
import types
import clipboard
from storage import Storage
from typing import Dict, Any
from receiver import Receiver
from utils import ExitException


class Calc(Receiver):
    def __init__(self, storage: Storage):
        super().__init__(storage, "Calc")
        self.locals_dict = vars(math)
        self.globals_dict: Dict[str, Any] = {"__builtins__": {}}

    def on_init(self):
        pass

    def on_input(self, text: str):
        self.reset_lines()
        text = text.strip()
        if text == "":
            return

        try:
            processed_text = text.replace("^", "**")
            answer = eval(processed_text, self.globals_dict, self.locals_dict)
            if isinstance(answer, types.BuiltinFunctionType):
                return

            self.add_line(f"= {answer}", str(answer), filtering=False, icon="accessories-calculator")
        except Exception:
            pass

    def on_enter(self, id_text: str, text: str):
        clipboard.set_clipboard(id_text)
        raise ExitException()
