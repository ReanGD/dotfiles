import os
import subprocess
from writer import Writer
from receiver import Receiver
from utils import ExitException, get_icon


class CustomMenu(Receiver):
    def __init__(self, writer: Writer):
        super().__init__(writer, "CustomMenu")

    def on_init(self):
        session_id = os.environ.get("XDG_SESSION_ID", None)
        if session_id is not None:
            self.add_line("Lock screen", f"loginctl lock-session {session_id}", icon=get_icon("lock.svg"))
            self.add_line("Logout", f"loginctl terminate-session {session_id}", icon=get_icon("logout.svg"))
        self.add_line("Reboot", "systemctl reboot", icon=get_icon("reboot.svg"))
        self.add_line("Poweroff", "systemctl poweroff", icon=get_icon("poweroff.svg"))
        self.add_line("Suspend", "systemctl suspend", icon=get_icon("suspend.svg"))
        self.add_line("Hibernate", "systemctl hibernate", icon=get_icon("hibernate.svg"))

    def on_enter(self, id_text: str, text: str):
        subprocess.run(id_text.split(), shell=False, check=True)
        raise ExitException()
