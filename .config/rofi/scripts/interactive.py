#!/bin/python

import sys
import json
import math
import types


class Rofi:
	def __init__(self, log_path):
		self.log_handler = open("/home/rean/tmp/rofi/log.txt", "w")

	def send_raw(self, msg):
		sys.stdout.write(json.dumps(msg) +  "\n")
		sys.stdout.flush()

	def send(self, header=None, lines=None, prompt=None, action=None):
		msg = {}
		if header is not None:
			msg["message"] = header

		if lines is not None:
			msg["lines"] = lines

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


class Calc(Rofi):
	def __init__(self):
		self.globals_dict = { "__builtins__" : {} }
		self.locals_dict = vars(math)
		super(Calc, self).__init__("/home/rean/tmp/rofi/log.txt")

	def on_input(self, text):
		if text == "":
			self.send(header = "Result:")
			return

		answer = ""
		try:
			result = eval(text.replace("^","**"), self.globals_dict, self.locals_dict)
			if not isinstance(result, types.BuiltinFunctionType):
				answer = result
		except Exception:
			pass

		self.send(header = f"Result: {answer}")


if __name__ == '__main__':
	try:
		obj = Calc()
		obj.run("calc")
	except Exception as e:
		print('Error exit: ' + str(e))
