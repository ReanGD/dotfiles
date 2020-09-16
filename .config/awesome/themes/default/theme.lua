local gears = require('gears')
local beautiful = require('beautiful')

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local theme = {}

local gfs = gears.filesystem
local dpi = beautiful.xresources.apply_dpi

-- Module functions
--------------------------------------------------------------------------------
function theme:solarized_colors()
    self.bg_normal = "#002b36ff"
    self.bg_focus  = "#586e75ff"
    self.bg_urgent = self.bg_normal
    self.fg_normal = "#93a1a1ff"
    self.fg_focus  = "#fdf6e3ff"
    self.fg_urgent = "#dc322fff"
end

-- see https://material.io/design/color/the-color-system.html#tools-for-picking-colors
function theme:custom_colors()
    self.bg_normal = "#00334d"
    self.bg_focus  = "#375c79"
    self.bg_urgent = self.bg_normal
    self.fg_normal = "#bebebe"
    self.fg_focus  = "#ffffffaa"
    self.fg_urgent = "#dc322fff"
end

function theme:client()
    self.useless_gap   = 0
    self.border_width  = dpi(1)
    self.border_normal = "#000000"
    self.border_focus  = self.bg_focus
end

function theme:tasklist()
    self.tasklist_bg_focus = self.bg_normal
    self.tasklist_fg_focus = self.fg_normal
end

function theme:layout()
    local default_themes_path = gfs.get_themes_dir().."default/"
    local l = default_themes_path .. "layouts/"

    self.layout_fairh      = l .. "fairhw.png"
    self.layout_fairv      = l .. "fairvw.png"
    self.layout_floating   = l .. "floatingw.png"
    self.layout_magnifier  = l .. "magnifierw.png"
    self.layout_max        = l .. "maxw.png"
    self.layout_fullscreen = l .. "fullscreenw.png"
    self.layout_tilebottom = l .. "tilebottomw.png"
    self.layout_tileleft   = l .. "tileleftw.png"
    self.layout_tile       = l .. "tilew.png"
    self.layout_tiletop    = l .. "tiletopw.png"
    self.layout_spiral     = l .. "spiralw.png"
    self.layout_dwindle    = l .. "dwindlew.png"
    self.layout_cornernw   = l .. "cornernww.png"
    self.layout_cornerne   = l .. "cornernew.png"
    self.layout_cornersw   = l .. "cornersww.png"
    self.layout_cornerse   = l .. "cornersew.png"
end

-- Constructor
--------------------------------------------------------------------------------
function theme:init()
    -- Common
    self.font = "Inter Medium 10"
    self.form_font = "Inter Medium 12" -- Custom
    self.path = gfs.get_configuration_dir() .. "themes/default"

    self.icon_theme = "/usr/share/icons/mate"
    self.wallpaper = self.path .. "/wallpapers/morning-wallpaper.jpg"

    -- self:solarized_colors()
    self:custom_colors()
    self:client()
    self:tasklist()
    self:layout()
end

theme:init()

return theme
