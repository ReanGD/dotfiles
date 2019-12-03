local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup").widget

local keys = { client_keys = {}, client_buttons = {}}

local function windows_go_back()
    awful.client.focus.history.previous()
    if client.focus then
        client.focus:raise()
    end
end

local function launcher_system_menu(env)
    awful.spawn(env.scripts_dir .. "run_menu.sh")
end

local function tag_numkey(i, modkey, action)
    return awful.key(modkey, "#" .. i + 9,
        function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
                action(tag)
            end
        end
    )
end

local function client_numkey(i, modkey, action)
    return awful.key(modkey, "#" .. i + 9,
        function ()
            if client.focus then
                local tag = client.focus.screen.tags[i]
                if tag then
                    action(tag)
                end
            end
        end
    )
end


function keys:init(env)
    local M = { env.modkey }
    local SM = { env.modkey, "Shift" }
    local CM = { env.modkey, "Control" }
    local SCM = { env.modkey, "Shift", "Control" }
    local Key = awful.key
    local Button = awful.button

    local root_keys = gears.table.join(
        -- Tags
        Key(M, "Left",  awful.tag.viewprev,
            {group = "Tag", description = "View previous"}),
        Key(M, "Right", awful.tag.viewnext,
            {group = "Tag", description = "View next"}),
        Key(M, "Tab",   awful.tag.history.restore,
            {group = "Tag", description = "Go back"}),

        -- Windows
        Key(M,  "j", function () awful.client.focus.byidx(1) end,
            {group = "Windows", description = "Focus next by index"}),
        Key(SM, "j", function () awful.client.swap.byidx(1) end,
            {group = "Windows", description = "Swap with next window by index"}),
        Key(M,  "k", function () awful.client.focus.byidx(-1) end,
            {group = "Windows", description = "Focus previous by index"}),
        Key(SM, "k", function () awful.client.swap.byidx( -1) end,
            {group = "Windows", description = "Swap with previous window by index"}),
        Key(M,  "p", function () windows_go_back() end,
            {group = "Windows", description = "Go back"}),

        -- Windows size
        Key(M, "l",  function () awful.tag.incmwfact( 0.05) end,
            {group = "Windows", description = "Increase master width factor"}),
        Key(M, "h",  function () awful.tag.incmwfact(-0.05) end,
            {group = "Windows", description = "Decrease master width factor"}),

        -- Awesome
        Key(SCM, "r", awesome.restart,
            {group = "Awesome", description = "Reload awesome"}),
        Key(SCM, "q", awesome.quit,
            {group = "Awesome", description = "Quit awesome"}),

        -- Layout
        Key(M, "space", function () awful.layout.inc(1) end,
            {group = "Layout", description = "Select next"}),

        -- Launcher
        Key(M, "s",      hotkeys_popup.show_help,
            {group = "Launcher", description = "Show help"}),
        Key(M, "Return", function () awful.spawn(env.terminal) end,
            {group = "Launcher", description = "Run terminal"}),
        Key(M,  "e",     function () awful.spawn("emacs") end,
            {group = "Launcher", description = "Run emacs"}),
        Key(SM, "r",     function () awful.spawn("pkill sleep") end,
            {group = "Launcher", description = "Run pkill sleep"}),
        Key(M,  "r",     function () awful.spawn("rofi -show run") end,
            {group = "Launcher", description = "Open program menu"}),
        Key(SM, "r",     function () launcher_system_menu(env) end,
            {group = "Launcher", description = "Open system menu"})
    )

    for i = 1, 9 do
        root_keys = gears.table.join(root_keys,
            tag_numkey(i, M,     function(t) t:view_only() end),
            tag_numkey(i, CM,    function(t) awful.tag.viewtoggle(t) end),
            client_numkey(i, SM, function(t) client.focus:move_to_tag(t) end)
        )
    end

    self.client_keys = gears.table.join(
        Key(M,  "f",    function(c) c.fullscreen = not c.fullscreen c:raise() end,
            {group = "Windows", description = "Toggle fullscreen"}),
        Key(SM, "q",    function(c) c:kill() end,
            {group = "Windows", description = "Close"}),
        Key(M,  "m",     function(c) c.maximized = not c.maximized c:raise() end ,
            {group = "Windows", description = "(Un)Maximize"}),
        Key(CM, "space", awful.client.floating.toggle,
            {group = "Windows", description = "Toggle floating"}),
        Key(M, "Print", function() awful.spawn("scrot --focused --exec 'mv $f ~/tmp'") end,
            {group = "Windows", description = "Print screen"})
    )

    self.client_buttons = gears.table.join(
        Button({}, 1, function (c) client.focus = c; c:raise() end),
        Button(M,  1, awful.mouse.client.move),
        Button(M,  3, awful.mouse.client.resize)
    )

    root.keys(root_keys)
end

return keys
