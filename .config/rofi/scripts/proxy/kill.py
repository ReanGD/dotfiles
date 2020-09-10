import os
import psutil
import getpass
from writer import Writer
from receiver import Receiver
from utils import ExitException


class Kill(Receiver):
    def __init__(self, writer: Writer):
        super().__init__(writer, group="Kill", activate_word="kill", prompt="kill")
        self._current_user = getpass.getuser()

    def on_init(self):
        attrs = ["pid", "cmdline", "username"]
        for pid, info in [(it.pid, it.info)
                          for it in psutil.process_iter(attrs)
                          if it.info['username'] == self._current_user]:
            cmd = " ".join(info['cmdline'])
            self.add_line(f"{pid} {cmd}", str(pid))

    def on_input(self, text: str):
        pass

    def on_enter(self, id_text: str, text: str):
        pid = int(id_text)
        if pid == os.getpid():
            return

        self._kill_with_subprocesses(pid)
        raise ExitException()

    def _kill_with_subprocesses(self, pid):
        parent = psutil.Process(pid)
        processes = parent.children(recursive=True)
        processes.append(parent)
        for p in processes:
            p.terminate()

        gone, alive = psutil.wait_procs(processes, timeout=5)
        for p in alive:
            p.kill()
