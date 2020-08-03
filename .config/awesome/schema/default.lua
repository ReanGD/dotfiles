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
clock = require("widget.clock")
volume = require("widget.volume")
systray = require("widget.systray")
taglist = require("widget.taglist")
tasklist = require("widget.tasklist")
keyboard = require("widget.keyboard")
layoutbox = require("widget.layoutbox")

-- Custom library
timestamp = require("src.timestamp")

-- Setup theme and environment vars
--------------------------------------------------------------------------------
env:init({theme="default"})

awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.max
}


clock:init()
volume:init({mixer = env.mixer})
systray:init()
taglist:init()
tasklist:init()
keyboard:init()
layoutbox:init()

awful.screen.connect_for_each_screen(function(s)
    -- set wallpaper
    env:wallpaper_setup(s)

    awful.tag({ "web", "doc", "devel", "cmdr", "media", "custom" }, s,
              { awful.layout.layouts[3],
                awful.layout.layouts[1],
                awful.layout.layouts[3],
                awful.layout.layouts[3],
                awful.layout.layouts[2],
                awful.layout.layouts[2] })


    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.button({ }, 1, function () awful.layout.inc( 1) end))

    -- Create the wibox
    s.bar = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    s.bar:setup {
        layout = wibox.layout.align.horizontal,
        {
            layout = wibox.layout.fixed.horizontal,
            layoutbox:widget(s),
            taglist:widget(s),
        },
        tasklist:widget(s),
        {
            layout = wibox.layout.fixed.horizontal,
            systray.widget,
            volume.widget,
            keyboard.widget,
            clock.widget,
        },
    }
end)

local hotkeys = require("config.keys-config")
hotkeys:init(env)

local rules = require("config.rules-config")
rules:init(hotkeys)

local signals = require("config.signals-config")
signals:init()

local autostart = require("config.autostart-config")
if timestamp.is_startup() then
  autostart.run(env)
end
