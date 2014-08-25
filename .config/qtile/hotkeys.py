from libqtile.config import Key
from libqtile.command import lazy
from dmenu import DMenu
from pulseaudio import inc_volume, dec_volume


def hide_show_bar(qtile):
    bar = qtile.currentScreen.top
    if bar.size == 0:
        bar.size = 30
        bar.window.unhide()
    else:
        bar.size = 0
        bar.window.hide()
    qtile.currentGroup.layoutAll()


def get_keyboard_hotkey(mod):
    return [
        Key([mod], "k",
            lazy.layout.down()),
        Key([mod], "j",
            lazy.layout.up()),
        Key([mod, "shift"], "k",
            lazy.layout.shuffle_down()),
        Key([mod, "shift"], "j",
            lazy.layout.shuffle_up()),
        Key([mod], "l",
            lazy.layout.grow()),
        Key([mod], "h",
            lazy.layout.shrink()),
        Key([mod], "n",
            lazy.layout.normalize()),
        Key([mod], "o",
            lazy.layout.maximize()),
        Key([mod], "space",
            lazy.nextlayout()),
        Key([mod], "r",
            lazy.function(lambda qtile: DMenu().run(True))),
        Key([mod, "shift"], "r",
            lazy.function(lambda qtile: DMenu().run(False))),
        Key([mod], "equal",
            lazy.function(inc_volume)),
        Key([mod], "minus",
            lazy.function(dec_volume)),
        Key([mod], "Return",
            lazy.spawn("urxvt")),
        Key([mod, "shift"], "q",
            lazy.window.kill()),
        Key([mod, "shift", "control"], "r",
            lazy.restart()),
        Key([mod, "shift", "control"], "q",
            lazy.shutdown()),
        Key([mod, "shift"], "d",
            lazy.function(hide_show_bar)),
    ]


def get_mouse_hotkey(mod):
    return []


def get_group_hotkey(idx, gr_name, mod):
    return [
        Key([mod], str(idx+1), lazy.group[gr_name].toscreen()),
        Key([mod, "shift"], str(idx+1), lazy.window.togroup(gr_name)),
    ]
