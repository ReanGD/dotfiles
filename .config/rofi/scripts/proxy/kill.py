import os
import psutil
import getpass
from storage import Storage
from typing import Dict, List
from receiver import Receiver
from utils import ExitException


class Kill(Receiver):
    def __init__(self, storage: Storage):
        super().__init__(storage, group="Kill", activate_word="kill", prompt="kill")
        self._current_user = getpass.getuser()
        self._cache: Dict[int, psutil.Process] = {}
        self._roots: List[psutil.Process] = []

    @staticmethod
    def _fill_info(proc: psutil.Process):
        info = proc.as_dict(attrs=["ppid", "cmdline", "username"], ad_value=None)
        proc.v_ppid = info["ppid"]
        proc.v_username = info["username"]
        proc.v_cmdline = " ".join(info['cmdline'])
        proc.v_children = []

        return proc

    def _add_to_cache(self, pid: int):
        proc = psutil.Process(pid)
        self._cache[proc.pid] = Kill._fill_info(proc)

    def _remove_from_cache(self, pid: int):
        self._cache.pop(pid, None)

    def _update_cache(self):
        cache_pins = set(self._cache.keys())
        current_pins = set(psutil.pids())

        del_pids = cache_pins - current_pins
        for pid in del_pids:
            self._remove_from_cache(pid)

        for pin, proc in self._cache.items():
            try:
                if proc.is_running():
                    Kill._fill_info(proc)
                else:
                    # update reused PID
                    self._add_to_cache(pid)
            except psutil.NoSuchProcess:
                self._remove_from_cache(pid)
            except psutil.AccessDenied:
                raise

        new_pids = current_pins - cache_pins
        for pid in new_pids:
            try:
                self._add_to_cache(pid)
            except psutil.AccessDenied:
                pass

    def _fill_parents(self):
        self._roots = []
        for proc in self._cache.values():
            if proc.v_ppid == 0:
                self._roots.append(proc)
            else:
                self._cache[proc.v_ppid].v_children.append(proc)

    def _get_processes(self, offset: int, parent: psutil.Process):
        if offset == 0:
            yield "", parent
        elif offset == 1:
            yield "├─", parent
        else:
            yield " "*(offset * 2) + "├─", parent
        for child_proc in parent.v_children:
            for offset_str, proc in self._get_processes(offset + 1, child_proc):
                yield offset_str, proc

    def on_init(self):
        self._update_cache()
        self._fill_parents()
        for root_proc in self._roots:
            for offset, proc in self._get_processes(0, root_proc):
                self.add_line(f"{offset}{proc.v_cmdline} ?", str(proc.pid))
                # self.add_line(f"{int(proc.pid):10} {offset}{proc.v_cmdline}", str(proc.pid))

        # for pid, proc in self._cache.items():
        #     if proc.v_username == self._current_user:
        #         self.add_line(f"{pid} {proc.v_cmdline}", str(pid))

    def on_input(self, text: str):
        pass

    def on_enter(self, id_text: str, text: str):
        pid = int(id_text)
        if pid == os.getpid():
            return

        self._kill_with_subprocesses(pid)
        raise ExitException()

    def _kill_with_subprocesses(self, pid: int):
        parent = psutil.Process(pid)
        processes = parent.children(recursive=True)
        processes.append(parent)
        for p in processes:
            p.terminate()

        gone, alive = psutil.wait_procs(processes, timeout=5)
        for p in alive:
            p.kill()
