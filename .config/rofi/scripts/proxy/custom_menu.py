import os
import subprocess
from storage import Storage
from receiver import Receiver
from utils import ExitException


class CustomMenu(Receiver):
    def __init__(self, storage: Storage):
        super().__init__(storage, "CustomMenu")

    def on_init(self):
        session_id = os.environ.get("XDG_SESSION_ID", None)
        if session_id is not None:
            self.add_line("Lock screen", f"loginctl lock-session {session_id}", icon=self.resolve_icon("lock"))
        self.add_line("Reboot", "systemctl reboot", icon=self.resolve_icon("reboot"))
        self.add_line("Poweroff", "systemctl poweroff", icon=self.resolve_icon("poweroff"))
        self.add_line("Suspend", "systemctl suspend", icon=self.resolve_icon("suspend"))
        self.add_line("Hibernate", "systemctl hibernate", icon=self.resolve_icon("hibernate"))
        if session_id is not None:
            self.add_line("Logout", f"loginctl terminate-session {session_id}", icon=self.resolve_icon("logout"))

    def on_enter(self, id_text: str, text: str):
        subprocess.run(id_text.split(), shell=False, check=True)
        raise ExitException()
