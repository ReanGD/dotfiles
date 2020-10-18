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

            icon = "accessories-calculator"
            dec_answer = answer
            self.add_line(f"<tt>Dec {dec_answer}</tt>", str(dec_answer), markup=True, filtering=False, icon=icon)
            hex_answer = hex(answer)
            self.add_line(f"<tt>Hex {hex_answer}</tt>", str(hex_answer), markup=True, filtering=False, icon=icon)
            bin_answer = bin(answer)
            self.add_line(f"<tt>Bin {bin_answer}</tt>", str(bin_answer), markup=True, filtering=False, icon=icon)
        except Exception:
            pass

    def on_enter(self, id_text: str, text: str):
        clipboard.set_clipboard(id_text)
        raise ExitException()
