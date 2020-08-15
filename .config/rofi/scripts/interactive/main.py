#!/bin/python

import os
import sys
import json
import logging

from calc import Calc
from message import Message
from translate import Translate


class Runner:
    def __init__(self):
        self.calc = Calc()
        self.translate = Translate()
        self.log_handler = open(self.get_log_path(), "w")

    def get_log_path(self):
        home_dir = os.getenv("HOME", None)
        if home_dir is None:
            raise Exception("not found HOME env")

        xdg_data_home = os.getenv('XDG_DATA_HOME', os.path.join(home_dir, ".local", "share"))
        return os.path.join(xdg_data_home, "rofi", "interactive.log")

    def send(self, msg: Message):
        text = json.dumps(msg.build()) + "\n"
        sys.stdout.write(text)
        sys.stdout.flush()
        self.log("<< " + text)

    def log(self, text: str):
        self.log_handler.write(text)
        self.log_handler.flush()

    def on_input(self, user_text: str) -> Message:
        answer = Message()
        if self.calc.on_input(user_text, answer):
            return answer

        self.translate.on_input(user_text, answer)
        return answer

    def on_enter(self, user_text: str):
        if self.calc.on_enter(user_text):
            return

        if self.translate.on_enter(user_text):
            return

    def run(self):
        init_msg = Message().set_action(is_send=True).set_prompt("interactive")
        self.send(init_msg)

        for line in sys.stdin:
            self.log(">> " + line)
            user_msg = json.loads(line)
            msg_name = user_msg["name"]
            if msg_name == "input change":
                self.send(self.on_input(user_msg["value"]))
            elif msg_name == "select entry":
                self.on_enter(user_msg["value"])


if __name__ == '__main__':
    try:
        runner = Runner()
        runner.run()
    except Exception as e:
        print('Error exit: ' + str(e))
