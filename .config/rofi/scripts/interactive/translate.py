from message import Message
from googletrans import Translator
from clipboard import to_clipboard


class Translate:
    def __init__(self):
        self.translator = Translator(service_urls=["translate.google.ru"])
        self.lines = {}

    def add_line(self, user_text: str, value: str, msg: Message):
        msg.add_line(user_text)
        self.lines[user_text] = value

    def on_input(self, user_text: str, msg: Message) -> bool:
        self.lines = {}
        if len(user_text) <= 1:
            return False

        dst_lang = "ru"
        src_lang = self.translator.detect(user_text).lang
        if src_lang == "ru":
            dst_lang = "en"

        answer = self.translator.translate(user_text, dest=dst_lang).text
        self.add_line(f"{src_lang}->{dst_lang}: {answer}", answer, msg)

        return True

    def on_enter(self, user_text: str) -> bool:
        value = self.lines.get(user_text, None)
        if value is None:
            return False

        to_clipboard(value)
        return True
