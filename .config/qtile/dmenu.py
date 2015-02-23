#!/usr/bin/python2

import os
import shelve
from subprocess import Popen, PIPE
from itertools import chain


class DMenu:
    def __init__(self):
        self._db_path = os.getenv("HOME") + "/.config/qtile/files_rating.db"
        self._db = None
        self._virtual_commands = {
            "reboot": "systemctl reboot",
            "poweroff": "systemctl poweroff",
            "suspend": "systemctl suspend",
            "hibernate": "systemctl hibernate"}

    def _open_db(self):
        self._db = shelve.open(self._db_path)

    def _update_db(self, val):
        self._db[val] = self._db.get(val, 0) + 1
        self._db.sync()

    def _close_db(self):
        self._db.close()

    def _get_cmdline(self):
        cmd = "dmenu"
        font = "-fn 'droid sans mono'"
        params = "-t -i -l 10"
        colors = "-nb '#1D1D1D' -nf '#EFEFEF' -sb '#445444' -sf '#EFEFEF'"
        return " ".join([cmd, font, params, colors])

    def _run_dmenu(self, stdin_list, is_program):
        p = Popen(self._get_cmdline(), stdin=PIPE, stdout=PIPE, bufsize=1, shell=True)
        if is_program:
            sort_func = lambda x: (-self._db.get(x, 0), x)
        else:
            sort_func = lambda x: (-self._db.get("__" + x + "__", 0), x)
        stdin_sorted = "\n".join(sorted(stdin_list, key = sort_func))
        output = p.communicate(stdin_sorted)[0].strip()
        if len(output) != 0:
            self._update_db(output if is_program else "__" + output + "__")

        return output

    def _get_program_data(self):
        env_path = [path for path in os.environ['PATH'].split(':') if os.path.isdir(path)]
        return list(set(chain.from_iterable([os.listdir(it) for it in env_path])))

    def _run_program(self, output):
        if len(output) != 0:
            Popen(output, shell=True)

    def _get_virtual_data(self):
        return self._virtual_commands.keys()

    def _run_virtual(self, output):
        if len(output) != 0 and output in self._virtual_commands.keys():
            Popen(self._virtual_commands[output], shell=True)

    def run(self, is_program):
        self._open_db()

        if is_program:
            self._run_program(self._run_dmenu(self._get_program_data(), is_program))
        else:
            self._run_virtual(self._run_dmenu(self._get_virtual_data(), is_program))

        self._close_db()


if __name__ == '__main__':
    DMenu().run(False)
