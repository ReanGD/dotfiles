from libqtile.config import Screen, Group
from libqtile import layout, bar, widget, hook
from helpers import WindowHelper, RunHelper
import hotkeys
from pulseaudio import PulseAudio
from battery import Battery
from metrics import Metrics


mod = "mod4"

keys = hotkeys.get_keyboard_hotkey(mod)
mouse = hotkeys.get_mouse_hotkey(mod)

groups_config = [
    {"key": "web",    "name": "1:web",  "layout": "max"},
    {"key": "doc",    "name": "2:doc",  "layout": "monadtall"},
    {"key": "devel",  "name": "3:devel",  "layout": "max"},
    {"key": "cmdr",   "name": "4:cmdr",  "layout": "max"},
    {"key": "media",  "name": "5:media",  "layout": "max"},
    {"key": "custom", "name": "6:custom",  "layout": "max"}
]

groups = [Group(it["name"], layout=it["layout"]) for it in groups_config]
groups_map = {it["key"]: it["name"] for it in groups_config}

for idx, gr in enumerate(groups):
    keys += hotkeys.get_group_hotkey(idx, gr.name, mod)


dgroups_key_binder = None
dgroups_app_rules = []

border = dict(
    border_normal='#808080',
    border_width=2,
)

layouts = [
    layout.Max(),
    layout.MonadTall(**border),
    # layout.TreeTab()
    # layout.RatioTile()
    # layout.Stack(stacks=2),
    # layout.Zoomy()
    # layout.Tile()
]


color_scheme = dict(foreground="839496", background="1D1D1D")
# 002b36

group_bar = dict(fontsize=14, font="Ubuntu Mono")
title_bar = dict(fontsize=15, font="Ubuntu")
right_bar = dict(fontsize=13, font="Ubuntu Condensend")

group_bar.update(color_scheme)
title_bar.update(color_scheme)
right_bar.update(color_scheme)


class KeyboardLayout(widget.KeyboardLayout):
    def button_press(self, x, y, button):
        pass


def get_bar():
    return bar.Bar([
        widget.GroupBox(**group_bar),
        widget.Prompt(**title_bar),
        widget.WindowName(**title_bar),
        Battery(update_delay=5, **right_bar),
        Metrics(**right_bar),
        PulseAudio(**right_bar),
        widget.Sep(**color_scheme),
        KeyboardLayout(configured_keyboards=['us', 'ru'], **right_bar),
        widget.Sep(**color_scheme),
        widget.CurrentLayout(**right_bar),
        widget.Sep(**color_scheme),
        widget.Systray(**color_scheme),
        widget.Clock(format='%a %d.%m.%y %H:%M', **right_bar),
    ], 30)


screens = [Screen(top=get_bar())]


main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating()
mouse = ()
auto_fullscreen = True
widget_defaults = {}


# def set_layout(val):
#     import subprocess
#     subprocess.call(['dbus-send',
#                      '--dest=ru.gentoo.KbddService',
#                      '/ru/gentoo/KbddService',
#                      'ru.gentoo.kbdd.set_layout',
#                      'uint32:' + str(val)])


# def write(text):
#     open("/home/rean/123.txt", "a").write(text + '\n')


# class UserProps(object):
#     def __init__(self, qtile):
#         if 'user_props' not in qtile.__dict__:
#             qtile.user_props = {}
#         self._user_props = qtile.user_props

#     @property
#     def last_window(self):
#         return self._user_props.get('last_window', None)

#     @last_window.setter
#     def last_window(self, value):
#         self._user_props['last_window'] = value


# @hook.subscribe.client_focus
# def on_focus(window):
#     return
#     user_props = UserProps(window.qtile)
#     if user_props.last_window == window.window.wid:
#         return
#     user_props.last_window = window.window.wid

#     window_class = window.window.get_wm_class()[0]
#     open("/home/rean/123.txt", "a").write(window_class + '\n')
#     if window_class == "subl3":
#         set_layout(0)
#         window.focus(False)


# @hook.subscribe.client_new
# def dialogs(window):
#     dialog = window.window.get_wm_type() == 'dialog'
#     transient = window.window.get_wm_transient_for()
#     if dialog or transient:
#         window.floating = True


@hook.subscribe.client_new
def grouper(window):
    class GroupedParam:
        def __init__(self, group_key, first_only):
            self.group = groups_map[group_key]
            self.first_only = first_only

    windows = {'Navigator':  GroupedParam("web", True),
               'subl3':      GroupedParam("doc", True),
               'urxvt':      GroupedParam("doc", True),
               # "codeblocks": GroupedParam("devel", False),
               'doublecmd':  GroupedParam("cmdr", True),
               }
    w = WindowHelper(window)
    if w.get_wm_class() in windows.keys():
        grouped_param = windows[w.get_wm_class()]
        if not grouped_param.first_only or w.is_first():
            window.togroup(grouped_param.group)


@hook.subscribe.startup
def runner():
    r = RunHelper()
    r.run(["xsetroot", "-cursor_name", "left_ptr"])
