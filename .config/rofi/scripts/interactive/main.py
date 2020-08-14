#!/bin/python

import sys
import json

from calc import Calc
from message import Message
from translate import Translate


class Runner:
    def __init__(self):
        self.calc = Calc()
        self.translate = Translate()
        self.log_handler = open("/home/rean/tmp/rofi/log.txt", "w")

    def send(self, msg: Message):
        sys.stdout.write(json.dumps(msg.build()) + "\n")
        sys.stdout.flush()

    def log(self, text: str):
        self.log_handler.write(text)
        self.log_handler.flush()

    def on_input(self, user_text: str) -> Message:
        answer = Message()
        if self.calc.on_input(user_text, answer):
            return answer

        self.translate.on_input(user_text, answer)
        return answer

    def run(self):
        init_msg = Message().set_action(is_send=True).set_prompt("interactive")
        self.send(init_msg)

        for line in sys.stdin:
            self.log(line)
            user_msg = json.loads(line)
            if user_msg["name"] == "input change":
                self.send(self.on_input(user_msg["value"]))


if __name__ == '__main__':
    try:
        runner = Runner()
        runner.run()
    except Exception as e:
        print('Error exit: ' + str(e))
