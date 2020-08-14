#!/bin/python

from calc import Calc
from translate import Translate


def main():
	obj = Calc()
	obj.run("calc")


if __name__ == '__main__':
	try:
		main()
	except Exception as e:
		print('Error exit: ' + str(e))
