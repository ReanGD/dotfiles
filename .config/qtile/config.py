from os.path import expanduser
from libqtile.lazy import lazy
from libqtile import qtile, hook, layout
from libqtile.utils import guess_terminal
from libqtile.config import Click, Drag, Group, Key, Match, Screen

groups = [
    Group(name="web", layout="max"),
    Group(name="doc", layout="columns"),
    Group(name="devel", layout="columns"),
    Group(name="cmdr", layout="max"),
    Group(name="media", layout="columns"),
    Group(name="custom", layout="columns"),
    Group(name="bg", layout="columns"),
]

terminal = guess_terminal()
polybar_reload = expanduser("~/.config/polybar/launch.sh")

modkey = "mod4"
S = [ "shift" ]
C = [ "control" ]
M = [ modkey ]
SM = [ modkey, "shift" ]
CM = [ modkey, "control" ]
SCM = [ modkey, "shift", "control" ]

keys = [
    # https://docs.qtile.org/en/latest/manual/config/lazy.html

    # Root
    Key(SCM, "r", lazy.reload_config(), lazy.spawn(polybar_reload),
        desc = "Root: Reload config"),
    Key(SCM, "q", lazy.shutdown(),
        desc = "Root: Quit"),

    # Screen focus
    Key(M, "Left", lazy.screen.prev_group(),
        desc = "Screen focus: Go to left screen"),
    Key(M, "Right", lazy.screen.next_group(),
        desc = "Screen focus: Go to right screen"),
    Key(M, "Up", lazy.screen.next_group(),
        desc = "Screen focus: Go to up screen"),
    Key(M, "Down", lazy.screen.prev_group(),
        desc = "Screen focus: Go to down screen"),
    Key(M, "Tab", lazy.screen.toggle_group(),
        desc = "Screen focus: Go to prev screen"),

    # Window focus
    Key(M, "j", lazy.layout.left(),
        desc = "Window focus: Go to left window"),
    Key(M, "l", lazy.layout.right(),
        desc = "Window focus: Go to right window"),
    Key(M, "i", lazy.layout.up(),
        desc = "Window focus: Go to up window"),
    Key(M, "k", lazy.layout.down(),
        desc = "Window focus: Go to down window"),

    # Window position
    Key(SM, "j", lazy.layout.shuffle_left(),
        desc = "Window position: Move to left"),
    Key(SM, "l", lazy.layout.shuffle_right(),
        desc = "Window position: Move to right"),
    Key(SM, "i", lazy.layout.shuffle_up(),
        desc = "Window position: Move to up"),
    Key(SM, "k", lazy.layout.shuffle_down(),
        desc = "Window position: Move to down"),

    # Windows size
    Key(CM, "l", lazy.layout.grow_right(),
        desc = "Window size: Increase window width"),
    Key(CM, "j", lazy.layout.grow_left(),
        desc = "Window size: Decrease window width"),
    Key(CM, "i", lazy.layout.grow_up(),
        desc = "Window size: Increase window height"),
    Key(CM, "k", lazy.layout.grow_down(),
        desc = "Window size: Decrease window height"),
    Key(CM, "n", lazy.layout.normalize(),
        desc = "Window size: Reset all window sizes"),

    # Window operations
    Key(M,  "f", lazy.window.toggle_fullscreen(),
        desc="Window: Toggle fullscreen"),
    Key(M,  "m", lazy.window.toggle_maximize(),
        desc="Window: Toggle maximize"),
    Key(M,  "b", lazy.window.toggle_floating(),
        desc="Window: Toggle floating"),
    Key(SM, "q", lazy.window.kill(),
        desc="Window: Close"),

    # Layout
    Key(M,  "space", lazy.next_layout(),
        desc = "Layout: Select next"),
    Key(SM, "space", lazy.prev_layout(),
        desc = "Layout: Select next"),

    # Launcher
    Key(M, "Return", lazy.spawn(terminal),
        desc="Launch: terminal"),
    Key(SM, "r", lazy.spawncmd(),
        desc="Launch: Spawn a command using a prompt widget"),
]

for num, group in enumerate(groups):
    key = str(num + 1)
    name = group.name
    keys.extend([
        Key(M,  key, lazy.group[group.name].toscreen(),
            desc = f"Switch to group {name}"),
        Key(SM, key, lazy.window.togroup(name, switch_group=True),
            desc = f"Switch to & move focused window to group {name}",),
        ]
    )

mouse = [
    Drag(M, "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag(M, "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click(M, "Button2", lazy.window.bring_to_front()),
]

layouts = [
    layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"], border_width=4),
    layout.Bsp(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
    layout.Max(),
]

widget_defaults = dict(
    font="sans",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
        # By default we handle these events delayed to already improve performance, however your system might still be struggling
        # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
        x11_drag_polling_rate = 60,
    ),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = False
follow_mouse_focus = False
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        Match(wm_class="runify-ui"),  # ssh-askpass
        Match(role="TfrmViewer"),  # ssh-askpass
        Match(role="TfrmFindDlg"),  # ssh-askpass
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

@hook.subscribe.layout_change
def layout_change(layout, group):
    qtile.spawn(f'polybar-msg action "#subscriber.send.{layout.name}"')


@hook.subscribe.client_focus
def floating_dialogs(window):
    if not hasattr(window, 'fix') and window.window.get_wm_transient_for() is None and window.get_wm_class()[0] == 'transgui':
        window.fix = True
        window.floating = True
        window.floating = False

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
