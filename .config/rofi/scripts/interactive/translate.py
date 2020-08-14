from rofi import Rofi
from googletrans import Translator


class Translate(Rofi):
	def __init__(self):
		self.translator = Translator(service_urls=["translate.google.ru"])
		super(Translate, self).__init__("/home/rean/tmp/rofi/log.txt")

	def on_input(self, text):
		if len(text) <= 1:
			self.send(header = "Result:")
			return

		dst_lang = "ru"
		src_lang = self.translator.detect(text).lang
		if src_lang == "ru":
			dst_lang = "en"

		answer = self.translator.translate(text, dest=dst_lang).text

		self.send(lines = [f"<span>Result: <b>{answer}</b></span>\n<span>{src_lang}->{dst_lang}</span>"])

