-- Load modules
--------------------------------------------------------------------------------

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local vicious = require("vicious")

require("awful.autofocus")

-- Widget library
widget_clock = require('widget.clock')

-- Custom library
timestamp = require("src.timestamp")
local widget = require("widget")

-- Error handling
--------------------------------------------------------------------------------
require("std.error_check")

-- Setup theme and environment vars
--------------------------------------------------------------------------------
local env = require("std.env")
env:init{theme="default"}

awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.max
}

local w_textclock = widget.textclock(env)
local w_keyboard = widget.keyboard()
local w_systray = wibox.widget.systray()

awful.screen.connect_for_each_screen(function(s)
    -- set wallpaper
    env.wallpaper(s)
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

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all,
      gears.table.join( awful.button({ }, 1, function(t) t:view_only() end),
                        awful.button({ }, 3, awful.tag.viewtoggle)))

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.focused, {})

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    batwidget = wibox.widget.textbox()
    vicious.register(batwidget, vicious.widgets.bat, "<span font=\"Ubuntu 10\" color=\"#3CAA3C\"><b>  $1$2% ($3)</b></span>", 120, 'BAT0')

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            s.mylayoutbox,
            s.mytaglist,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            w_systray,
            batwidget,
            w_keyboard,
            w_textclock,
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
