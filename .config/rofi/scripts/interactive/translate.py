from message import Message
from googletrans import Translator


class Translate:
    def __init__(self):
        self.translator = Translator(service_urls=["translate.google.ru"])

    def on_input(self, user_text: str, msg: Message) -> bool:
        if len(user_text) <= 1:
            return False

        dst_lang = "ru"
        src_lang = self.translator.detect(user_text).lang
        if src_lang == "ru":
            dst_lang = "en"

        answer = self.translator.translate(user_text, dest=dst_lang).text
        msg.add_line(f"{src_lang}->{dst_lang}: {answer}")

        return True
