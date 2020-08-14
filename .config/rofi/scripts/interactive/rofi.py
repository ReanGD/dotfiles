import sys
import json


class Rofi:
    def __init__(self, log_path):
        self.log_handler = open("/home/rean/tmp/rofi/log.txt", "w")

    def send_raw(self, msg):
        sys.stdout.write(json.dumps(msg) + "\n")
        sys.stdout.flush()

    def send(self, header=None, lines=None, prompt=None, action=None):
        msg = {}
        if header is not None:
            msg["message"] = header

        if lines is not None:
            msg["lines"] = [{"text": it, "markup": True} for it in lines]

        if prompt is not None:
            msg["prompt"] = prompt

        if action is not None:
            msg["input action"] = action

        self.send_raw(msg)

    def log(self, msg):
        self.log_handler.write(msg)
        self.log_handler.flush()

    def run(self, prompt):
        self.send(prompt=prompt, action="send")
        for line in sys.stdin:
            self.log(line)
            msg = json.loads(line)
            if msg["name"] == "input change":
                self.on_input(msg["value"])

    def on_input(self, text):
        pass
