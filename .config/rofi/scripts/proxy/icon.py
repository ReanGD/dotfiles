import os


class Icon:
    def __init__(self):
        cfg_path = os.environ.get("XDG_CONFIG_HOME", None)
        if cfg_path is None:
            cfg_path = os.path.join(os.environ["HOME"], ".config")
        self._base = os.path.join(cfg_path, "rofi", "icons")

    def resolve(self, name: str, white: bool = True) -> str:
        if white:
            return os.path.join(self._base, name + "_white.svg")
        else:
            return os.path.join(self._base, name + "_black.svg")
