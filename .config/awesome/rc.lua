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
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
-- require("volume")
local hotkeys_popup = require("awful.hotkeys_popup").widget
require("awful.hotkeys_popup.keys.vim")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}


-- {{{ Variable definitions
os.setlocale(os.getenv("LANG"))
-- Themes define colours, icons, font and wallpapers.
-- /usr/share/awesome/themes/
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "subl3"

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

-- {{{ Key bindings
globalkeys = gears.table.join(
  -- Tags
  awful.key({ modkey,         }, "Left",   awful.tag.viewprev,
            {description = "view previous", group = "tag"}),
  awful.key({ modkey,         }, "Right",  awful.tag.viewnext,
            {description = "view next", group = "tag"}),
  awful.key({ modkey,         }, "Tab",    awful.tag.history.restore,
            {description = "go back", group = "tag"}),
  
  -- Windows
  awful.key({ modkey,         }, "j", function () awful.client.focus.byidx( 1) end,
      {description = "focus next by index", group = "windows"}),
  awful.key({ modkey, "Shift" }, "j", function () awful.client.swap.byidx(  1)    end,
            {description = "swap with next window by index", group = "windows"}),
  awful.key({ modkey,         }, "k", function () awful.client.focus.byidx(-1) end,
      {description = "focus previous by index", group = "windows"}),
  awful.key({ modkey, "Shift" }, "k", function () awful.client.swap.byidx( -1)    end,
            {description = "swap with previous window by index", group = "windows"}),
  awful.key({ modkey,         }, "p",
      function ()
          awful.client.focus.history.previous()
          if client.focus then client.focus:raise() end
      end,
      {description = "go back", group = "windows"}),

  -- Windows size
  awful.key({ modkey,         }, "l",     function () awful.tag.incmwfact( 0.05)          end,
            {description = "increase master width factor", group = "windows"}),
  awful.key({ modkey,         }, "h",     function () awful.tag.incmwfact(-0.05)          end,
            {description = "decrease master width factor", group = "windows"}),

  -- awesome
  awful.key({ modkey, "Shift", "Control" }, "r", awesome.restart,
            {description = "reload awesome", group = "awesome"}),
  awful.key({ modkey, "Shift", "Control" }, "q", awesome.quit,
            {description = "quit awesome", group = "awesome"}),
  awful.key({ modkey,                    }, "s", hotkeys_popup.show_help,
              {description="show help", group="awesome"}),

  awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
            {description = "select next", group = "layout"}),

  -- Standard program
  awful.key({ modkey,          }, "Return", function () awful.spawn(terminal) end,
            {description = "open a terminal", group = "launcher"}),
  awful.key({ modkey,          }, "e",      function () awful.spawn("emacs") end,
            {description = "open a emacs", group = "launcher"}),
  awful.key({ modkey,          }, "r",      function () awful.spawn("rofi -show run") end,
            {description = "open a rofi", group = "launcher"}),
  awful.key({ modkey,  "Shift" }, "r",      function () awful.spawn(awful.util.getdir("config") .. "/run_menu.sh") end,
            {description = "open a system rofi", group = "launcher"}),
  awful.key({ modkey,  "Shift" }, "r",      function () awful.spawn("pkill sleep") end,
            {description = "open a pkill sleep", group = "launcher"})
)

clientkeys = gears.table.join(
  awful.key({ modkey,           }, "f", function (c) c.fullscreen = not c.fullscreen c:raise() end,
            {description = "toggle fullscreen", group = "windows"}),
  awful.key({ modkey, "Shift"   }, "q", function (c) c:kill() end,
            {description = "close", group = "windows"}),
  awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle,
            {description = "toggle floating", group = "windows"}),
  awful.key({ modkey,           }, "m", function (c) c.maximized = not c.maximized c:raise() end ,
      {description = "(un)maximize", group = "windows"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end)
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
      },
      -- new client always open as slave
      callback = awful.client.setslave
    },

    -- Floating clients.
    { rule_any = {
        class = {
          "MPlayer",
          "open3d",
          "Pyorgcalendar",
          "pinentry",
          "gimp"
        }
    }, properties = { floating = true }},

    -- move to tag
    { rule = { class = "Firefox"   }, properties = { tag = "web" } },
    { rule = { class = "Subl3"     }, properties = { tag = "doc" } },
    { rule = { class = "Doublecmd" }, properties = { tag = "cmdr" } },

    { rule = { class = "Conky" },
      properties = {
        floating = true,
        sticky = true,
        ontop = false,
        focusable = false,
        size_hints = {"program_position", "program_size"} } },

    -- Add titlebars to dialogs
    { rule_any = {type = { "dialog" }
      }, properties = { titlebars_enabled = true }
    }
}
-- }}}

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

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
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

-- {{{ autoload
awful.spawn("/home/rean/.config/bin/firefox", { tag = "web" })
awful.spawn("subl3",                 { tag = "doc" })
awful.spawn("doublecmd",             { tag = "cmdr" })
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
