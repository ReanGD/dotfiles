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

require("config.errcheck-config")
timestamp = require("src.timestamp")

-- {{{ Variable definitions
os.setlocale(os.getenv("LANG"))
-- Themes define colours, icons, font and wallpapers.
-- /usr/share/awesome/themes/
beautiful.init(awful.util.getdir("config") .. "config/theme-config.lua")

-- Default modkey.
modkey = "Mod4"

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
-- Create a textclock widget
mytextclock = wibox.widget.textclock()


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
            mytextclock,
        },
    }
end)
-- }}}

local hotkeys = require("config.keys-config")
hotkeys:init(modkey)

local rules = require("config.rules-config")
rules:init(hotkeys)

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}



-- Autostart user applications
-----------------------------------------------------------------------------------------------------------------------
local autostart = require("config.autostart-config") -- load file with autostart application list

if timestamp.is_startup() then
  autostart.run()
end

-- }}}

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
-- datewidget = wibox.widget.textbox()
-- vicious.register(datewidget, vicious.widgets.date, "<span font=\"Ubuntu 11\" color=\"#C7D0CC\">%a. %B %d,  <span color=\"#D7E0DC\">%H:%M</span>   </span>", 60)

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
--     right_layout:add(datewidget)
-- end
-- -- }}}
