import clipboard
from storage import Storage
from receiver import Receiver
from utils import ExitException
from googletrans import Translator


class Translated:
    def __init__(self):
        self.src_lang = "en"
        self.dst_lang = "ru"
        self.src_text = ""
        self.dst_text = ""
        self.dst_synonyms = dict()
        self.dst_variants = list()


class Translate(Receiver):
    def __init__(self, storage: Storage):
        super().__init__(storage, "Translate")
        self._result = Translated()

        user_agent = "Mozilla/5.0 (X11; Linux x86_64; rv:81.0) Gecko/20100101 Firefox/81.0"
        self._translator = Translator(["translate.google.ru"], user_agent)

    def on_init(self):
        text = clipboard.get_primary_clipboard()
        if len(text) <= 1:
            text = clipboard.get_clipboard()
            if len(text) <= 1:
                self._fill_lines()
                return

        self._translate(text)
        self.writer.set_input(self._result.src_text)

    def on_input(self, text: str):
        if len(text) <= 1:
            self._fill_lines()
            return

        self._translate(text)

    def on_enter(self, id_text: str, text: str):
        if id_text == "#copy":
            clipboard.set_clipboard(self._result.dst_text)
            raise ExitException()

        if id_text != "#swap":
            clipboard.set_clipboard(id_text)
            raise ExitException()

        if len(self._result.dst_text) <= 1:
            self._fill_lines()
            return

        self._translate(self._result.dst_text)
        self.writer.set_input(self._result.src_text)

    def _fill_lines(self):
        self.reset_lines()
        self.add_line("Copy", "#copy", filtering=False, icon=self.resolve_icon("copy"))
        self.add_line("Swap", "#swap", filtering=False, icon=self.resolve_icon("swap"))
        for word, text in self._result.dst_synonyms.items():
            self.add_line(f"{text}", word, markup=True, filtering=False, icon=self.resolve_icon("copy"))
        for text in self._result.dst_variants:
            self.add_line(f"{text}", text, markup=False, filtering=False, icon=self.resolve_icon("copy"))

    def _translate(self, text: str):
        text = text.strip()
        if text != self._result.src_text:
            self._result = self._google_translate(text)
        src_lang = self._result.src_lang.upper()
        dst_lang = self._result.dst_lang.upper()
        markup_text = f"<b>{src_lang}</b>: {self._result.src_text}\r\r<b>{dst_lang}</b>: {self._result.dst_text}"
        self.writer.set_help(markup_text)
        self._fill_lines()

    TRANSLATION = 0
    SYNONYMS = 1
    SRC_LANGUAGE = 2
    POSSIBLE_TRANSLATIONS = 5
    CONFIDENCE = 6
    POSSIBLE_MISTAKES = 7
    SRC_POSSIBLE_LANGUAGES = 8
    DEFINITIONS = 12
    EXAMPLES = 13
    SEE_ALSO = 14

    def _google_translate(self, text: str) -> Translated:
        prev = self._result
        res = Translated()
        res.src_lang = prev.src_lang
        res.dst_lang = prev.dst_lang
        res.src_text = text

        cnt_err = 0
        while True:
            try:
                data = self._translator._translate(text, dest=res.dst_lang, src=res.src_lang, override=None)
                break
            except Exception as e:
                cnt_err += 1
                if cnt_err > 6:
                    raise e

        if res.src_lang not in data[self.SRC_POSSIBLE_LANGUAGES][0]:
            res.src_lang, res.dst_lang = res.dst_lang, res.src_lang
            data = self._translator._translate(text, dest=res.dst_lang, src=res.src_lang, override=None)

        res.dst_text = " ".join([d[0] for d in data[self.TRANSLATION] if d[0] is not None])

        if data[self.SYNONYMS] is not None:
            for group in data[self.SYNONYMS]:
                variants = group[2]
                for variant in variants:
                    word = variant[0]
                    translates = ",".join(variant[1])
                    word_text = f"<b>{word}</b> <span style='italic' weight='ultralight'>({translates})</span>"
                    res.dst_synonyms[word] = word_text

        if data[self.POSSIBLE_TRANSLATIONS] is not None:
            for group in data[self.POSSIBLE_TRANSLATIONS]:
                if group[0] != res.src_text:
                    continue

                res.dst_variants += [variant[0] for variant in group[2] if variant[0] != res.dst_text]

        return res
