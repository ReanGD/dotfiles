-- Load modules
--------------------------------------------------------------------------------

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

require("awful.autofocus")

-- Error handling
--------------------------------------------------------------------------------
require("std.error_check")

-- Std library
local env = require("std.env")

-- Widget library
local bar = require("widget.bar")
local clock = require("widget.clock")
local volume = require("widget.volume")
local systray = require("widget.systray")
local taglist = require("widget.taglist")
local tasklist = require("widget.tasklist")
local keyboard = require("widget.keyboard")
local layoutbox = require("widget.layoutbox")

-- Config modules
local autostart = require("cfg.autostart")

-- Setup theme and environment vars
--------------------------------------------------------------------------------
env:init({theme="default"})

awful.layout.layouts = {
    awful.layout.suit.tile.right,
    awful.layout.suit.tile.left,
    awful.layout.suit.max,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
}


bar:init({ bar_id = env.bar_id })
clock:init()
volume:init({ bar_id = env.bar_id, mixer = env.mixer })
systray:init()
taglist:init()
tasklist:init()
keyboard:init()
layoutbox:init({ modkey = env.modkey })

awful.screen.connect_for_each_screen(function(s)
    env:wallpaper_setup(s)

    awful.tag({ "web", "doc", "devel", "cmdr", "media", "custom" }, s,
              { awful.layout.suit.max,
                awful.layout.suit.tile.right,
                awful.layout.suit.max,
                awful.layout.suit.max,
                awful.layout.suit.tile.left,
                awful.layout.suit.tile.left })

    bar:create {
        screen = s,
        left = { layoutbox:widget(s), taglist:widget(s) },
        center = tasklist:widget(s),
        right = { systray.widget, volume.widget, keyboard.widget, clock.widget },
    }
end)

local hotkeys = require("config.keys-config")
hotkeys:init(env)

local rules = require("config.rules-config")
rules:init(hotkeys)

local signals = require("config.signals-config")
signals:init()

autostart:init()
