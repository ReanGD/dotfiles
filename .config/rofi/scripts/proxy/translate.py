import clipboard
from writer import Writer
from receiver import Receiver
from utils import ExitException
from googletrans import Translator


class Translate(Receiver):
    def __init__(self, writer: Writer):
        super().__init__(writer, "Translate")
        self.__src_text = ""
        self.__dst_text = ""
        self.__translator = Translator(service_urls=["translate.google.ru"])

    def on_init(self):
        self.reset_lines()
        self.add_line("Copy", "copy", filtering=False, icon="edit-copy")
        self.add_line("Swap", "swap", filtering=False, icon="go-up")

        text = clipboard.get_primary_clipboard()
        if len(text) <= 1:
            text = clipboard.get_clipboard()
            if len(text) <= 1:
                return

        self.__translate(text)
        self._writer.set_input(self.__src_text)

    def on_input(self, text: str):
        if len(text) <= 1:
            return

        self.__translate(text)

    def on_enter(self, id_text: str, text: str):
        if id_text == "copy":
            clipboard.set_clipboard(self.__dst_text)
            raise ExitException()

        if id_text == "swap":
            if len(self.__dst_text) <= 1:
                return

            self.__translate(self.__dst_text)
            self._writer.set_input(self.__src_text)

    def __translate(self, text: str):
        text = text.strip()
        dst_lang = "ru"
        src_lang = self.__translator.detect(text).lang
        if src_lang == "ru":
            dst_lang = "en"
        res = self.__translator.translate(text, dest=dst_lang)
        src_lang = res.src.upper()
        dst_lang = res.dest.upper()
        self.__src_text = res.origin
        self.__dst_text = res.text
        markup_text = f"<b>{src_lang}</b>: {self.__src_text}\r\r<b>{dst_lang}</b>: {self.__dst_text}"
        self._writer.set_help(markup_text)
