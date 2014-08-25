import re
import subprocess
from libqtile import hook


def _is_first(wm_class, wm_id):
    cnt = sum(it.window.get_wm_class() is not None and
              it.window.get_wm_class()[0] == wm_class and
              it.window.wid != wm_id
              for it in hook.qtile.windowMap.values())

    return cnt == 0


class WindowHelper:
    def __init__(self, window):
        self.window = window

    def get_wm_class(self):
        wm_class = self.window.window.get_wm_class()
        if wm_class is None:
            return None
        return wm_class[0]

    def get_id(self):
        return self.window.window.wid

    def is_first(self):
        wm_class = self.get_wm_class()
        wm_id = self.get_id()
        if wm_class is None:
            return None
        return _is_first(wm_class, wm_id)


class RunHelper:
    def __init__(self):
        pass

    def run(self, cmd_line):
        subprocess.Popen(cmd_line)

    def is_running(self, cmd_line):
        s = subprocess.Popen(["ps", "axw"], stdout=subprocess.PIPE)
        for x in s.stdout:
            if re.search(cmd_line[0], x):
                return True
        return False

    def run_once(self, cmd_line):
        if not self.is_running(cmd_line):
            self.run(cmd_line)
