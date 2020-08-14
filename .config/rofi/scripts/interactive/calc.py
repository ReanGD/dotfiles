import math
import types
from rofi import Rofi


class Calc(Rofi):
    def __init__(self):
        self.globals_dict = {"__builtins__": {}}
        self.locals_dict = vars(math)
        super(Calc, self).__init__("/home/rean/tmp/rofi/log.txt")

    def on_input(self, text):
        if text == "":
            self.send(header="Result:")
            return

        answer = ""
        try:
            result = eval(text.replace("^", "**"), self.globals_dict, self.locals_dict)
            if not isinstance(result, types.BuiltinFunctionType):
                answer = result
        except Exception:
            pass

        self.send(header=f"Result: {answer}")
