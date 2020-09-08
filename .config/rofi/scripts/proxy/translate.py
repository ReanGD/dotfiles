import clipboard
from writer import Writer
from receiver import Receiver
from utils import ExitException
from googletrans import Translator


class Translate(Receiver):
    def __init__(self, writer: Writer):
        super().__init__(writer, "Translate")
        self._src_text = ""
        self._dst_text = ""
        self._translator = Translator(service_urls=["translate.google.ru"])

    def on_init(self):
        self.reset_lines()
        self.add_line("Copy", "copy", filtering=False, icon="edit-copy")
        self.add_line("Swap", "swap", filtering=False, icon="go-up")

        text = clipboard.get_primary_clipboard()
        if len(text) <= 1:
            text = clipboard.get_clipboard()
            if len(text) <= 1:
                return

        self._translate(text)
        self.writer.set_input(self._src_text)

    def on_input(self, text: str):
        if len(text) <= 1:
            return

        self._translate(text)

    def on_enter(self, id_text: str, text: str):
        if id_text == "copy":
            clipboard.set_clipboard(self._dst_text)
            raise ExitException()

        if id_text == "swap":
            if len(self._dst_text) <= 1:
                return

            self._translate(self._dst_text)
            self.writer.set_input(self._src_text)

    def _translate(self, text: str):
        text = text.strip()
        dst_lang = "ru"
        src_lang = self._translator.detect(text).lang
        if src_lang == "ru":
            dst_lang = "en"
        res = self._translator.translate(text, dest=dst_lang)
        src_lang = res.src.upper()
        dst_lang = res.dest.upper()
        self._src_text = res.origin
        self._dst_text = res.text
        markup_text = f"<b>{src_lang}</b>: {self._src_text}\r\r<b>{dst_lang}</b>: {self._dst_text}"
        self.writer.set_help(markup_text)
