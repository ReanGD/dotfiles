from writer import Writer
from receiver import Receiver
from utils import ExitException


class Kill(Receiver):
    def __init__(self, writer: Writer):
        super().__init__(writer, group="Kill", activate_word="kill", prompt="kill")

    def on_init(self):
        self.add_line("process 1")

    def on_input(self, text: str):
        pass

    def on_enter(self, id_text: str, text: str):
        raise ExitException()
