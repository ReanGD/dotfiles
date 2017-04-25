-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
local vicious = require("vicious")
-- Theme handling library
local beautiful = require("beautiful")
-- require("volume")

timestamp = require("src.timestamp")
local widget = require("widget")

require("config.errcheck-config")

local env = require("config.env-config")
env:init()

-- {{{ Variable definitions

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,    
    awful.layout.suit.max
}
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()
-- {{{ Wibar

local w_textclock = widget.textclock(env)

awful.screen.connect_for_each_screen(function(s)
    -- Each screen has its own tag table.
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
            mykeyboardlayout,
            wibox.widget.systray(),
            w_textclock,
        },
    }
end)
-- }}}

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

-- -- {{{ !Conky
-- function get_conky()
--     local clients = client.get()
--     local conky = nil
--     local i = 1
--     while clients[i]
--     do
--         if clients[i].class == "Conky"
--         then
--             conky = clients[i]
--         end
--         i = i + 1
--     end
--     return conky
-- end
-- function raise_conky()
--     local conky = get_conky()
--     if conky
--     then
--         conky.ontop = true
--     end
-- end
-- function lower_conky()
--     local conky = get_conky()
--     if conky
--     then
--         conky.ontop = false
--     end
-- end
-- function toggle_conky()
--     local conky = get_conky()
--     if conky
--     then
--         if conky.ontop
--         then
--             conky.ontop = false
--         else
--             conky.ontop = true
--         end
--     end
-- end
-- -- }}}
-- -- {{{ Wibox
-- -- openwidget = wibox.widget.textbox()
-- -- openwidget:set_markup("<span font=\"Ubuntu 10\"> &lt; </span>")
-- -- closewidget = wibox.widget.textbox()
-- -- closewidget:set_markup("<span font=\"Ubuntu 10\"> &gt;  </span>")
-- -- cpuwidget = wibox.widget.textbox()
-- -- vicious.register(cpuwidget, vicious.widgets.cpu, "<span font=\"Ubuntu 10\" color=\"#3CAA3C\"><b>$1%</b></span>")
-- -- memwidget = wibox.widget.textbox()
-- -- vicious.register(memwidget, vicious.widgets.mem, "<span font=\"Ubuntu 10\" color=\"#9CC646\"><b>  $1% | $2MB</b></span>", 10)
-- -- batwidget = wibox.widget.textbox()
-- -- vicious.register(batwidget, vicious.widgets.bat, "<span font=\"Ubuntu 10\" color=\"#3CAA3C\"><b>  $1$2% ($3)</b></span>", 120, 'BAT0')

-- kbdwidget = wibox.widget.textbox()
-- kbdwidget.border_width = 1
-- kbdwidget.border_color = beautiful.fg_normal
-- kbdwidget:set_markup("<span font=\"Ubuntu 11\"><b>  US </b></span>")

-- kbdstrings = {[0] = "<span font=\"Ubuntu 11\"><b>  US </b></span>",
--               [1] = "<span font=\"Ubuntu 11\"><b>  RU </b></span>"}

-- dbus.request_name("session", "ru.gentoo.kbdd")
-- dbus.add_match("session", "interface='ru.gentoo.kbdd',member='layoutChanged'")
-- dbus.connect_signal("ru.gentoo.kbdd", function(...)
--     local data = {...}
--     local layout = data[2]
--     kbdwidget:set_markup(kbdstrings[layout])
--     end
-- )
-- hostname = io.popen("uname -n"):read()


-- for s = 1, screen.count() do
--     if s == 1 then right_layout:add(wibox.widget.systray()) end
--     right_layout:add(volume_widget)


--     right_layout:add(kbdwidget)
--     -- right_layout:add(openwidget)
--     -- right_layout:add(cpuwidget)
--     -- right_layout:add(memwidget)
--     -- if hostname == "archmini" then
--        -- right_layout:add(batwidget)
--     -- end
--     -- right_layout:add(closewidget)
-- end
-- -- }}}
