local gfs = require("gears.filesystem")
local xresources = require("beautiful.xresources")

local theme = {}

local themes_path = gfs.get_themes_dir().."default/"

theme.font          = "Ubuntu 11"
theme.taglist_font  = theme.font
theme.mono_font     = "Ubuntu Mono 11"

theme.bg_normal     = "#222222"
theme.bg_focus      = theme.bg_normal
theme.bg_urgent     = "#ff0000"
theme.bg_minimize   = "#444444"
theme.bg_systray    = theme.bg_normal
theme.fg_normal     = "#aaaaaa"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

theme.useless_gap   = 0
theme.border_width  = xresources.apply_dpi(1)
theme.border_normal = "#000000"
theme.border_focus  = "#535d6c"
theme.border_marked = "#91231c"

theme.tasklist_disable_icon = true
theme.taglist_squares_sel = nil
theme.taglist_squares_unsel = nil

theme.layout_max = themes_path.."layouts/maxw.png"
theme.layout_tileleft   = themes_path.."layouts/tileleftw.png"
theme.layout_tile = themes_path.."layouts/tilew.png"

theme.icon_theme = nil

return theme
