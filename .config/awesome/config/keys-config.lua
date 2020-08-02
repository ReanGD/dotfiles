-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup").widget

-- Widget library
volume = require("widget.volume")

local keys = { client_keys = {}, client_buttons = {}}

local function launcher_system_menu(env)
    awful.spawn(env.scripts_dir .. "run_menu.sh")
end

scrot_screenshot_path = os.getenv("HOME") .. "/tmp/$(date +%F_%T).png"
flameshot_screenshot_path = os.getenv("HOME") .. "/tmp/"

function screenshot_full()
    awful.util.spawn_with_shell("flameshot full -c -p " .. flameshot_screenshot_path)
    -- awful.util.spawn_with_shell("scrot " .. scrot_screenshot_path .. " --exec 'xclip -selection c -t image/png < $f'")
end

function screenshot_select()
    awful.util.spawn_with_shell("flameshot gui -p " .. flameshot_screenshot_path)
    -- awful.util.spawn_with_shell("sleep 0.5 && scrot --select " .. scrot_screenshot_path .. " --exec 'xclip -selection c -t image/png < $f'")
end

function screenshot_focused()
    awful.util.spawn_with_shell("scrot --focused " .. scrot_screenshot_path .. " --exec 'xclip -selection c -t image/png < $f'")
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


----------------------------------------------------------------------------------------------------------------------
-- Key support functions
----------------------------------------------------------------------------------------------------------------------
local redflat = require("redflat")

-- Change window focus
local function focus_switch(dir)
    return function()
        local screen = awful.screen.focused()
        local layout = awful.layout.get(screen)
        if layout == awful.layout.suit.max then
            if dir == "right" or dir == "up" then
                awful.client.focus.byidx(1)
            else
                awful.client.focus.byidx(-1)
            end
        else
            awful.client.focus.bydirection(dir, client.focus, true)
        end

		if client.focus then client.focus:raise() end
	end
end

-- Change window position
local function position_switch(dir)
    return function()
        awful.client.swap.bydirection(dir)
	end
end

function keys:init(env)
    local S = { "Shift" }
    local C = { "Control" }
    local M = { env.modkey }
    local SM = { env.modkey, "Shift" }
    local CM = { env.modkey, "Control" }
    local SCM = { env.modkey, "Shift", "Control" }
    local Key = awful.key
    local Button = awful.button

    local root_keys = gears.table.join(
        -- Window focus
        Key(M,  "j", focus_switch("left"),
            {group = "Window focus", description = "Go to left window"}),
        Key(M,  "l", focus_switch("right"),
            {group = "Window focus", description = "Go to right window"}),
        Key(M,  "i", focus_switch("up"),
            {group = "Window focus", description = "Go to up window"}),
        Key(M,  "k", focus_switch("down"),
            {group = "Window focus", description = "Go to down window"}),

        -- Window position
        Key(SM, "j", position_switch("left"),
            {group = "Window position", description = "Move to left"}),
        Key(SM, "l", position_switch("right"),
            {group = "Window position", description = "Move to right"}),
        Key(SM, "i", position_switch("up"),
            {group = "Window position", description = "Move to up"}),
        Key(SM, "k", position_switch("down"),
            {group = "Window position", description = "Move to down"}),

        -- Audio
        Key({}, "XF86AudioRaiseVolume", function () volume:volume_up(true) end,
            {group = "Hotkeys", description = "Increase volume up by 5%"}),
        Key(SM, "=", function () volume:volume_up(true) end,
            {group = "Hotkeys", description = "Increase volume up by 5%"}),
        Key({}, "XF86AudioLowerVolume", function () volume:volume_down(true) end,
            {group = "Hotkeys", description = "decrease volume up by 5%"}),
        Key(SM, "-", function () volume:volume_down(true) end,
            {group = "Hotkeys", description = "decrease volume up by 5%"}),
        Key({}, "XF86AudioMute", function () volume:toggle_mute(true) end,
            {group = "Hotkeys", description = "Toggle mute"}),
        Key(SM, "0", function () volume:toggle_mute(true) end,
            {group = "Hotkeys", description = "Toggle mute"}),
        -- Tags
        Key(M, "Left",  awful.tag.viewprev,
            {group = "Tag", description = "View previous"}),
        Key(M, "Right", awful.tag.viewnext,
            {group = "Tag", description = "View next"}),
        Key(M, "Tab",   awful.tag.history.restore,
            {group = "Tag", description = "Go back"}),

        -- Windows size
        -- Key(M, "l",  function () awful.tag.incmwfact( 0.05) end,
        --     {group = "Windows", description = "Increase master width factor"}),
        -- Key(M, "h",  function () awful.tag.incmwfact(-0.05) end,
        --     {group = "Windows", description = "Decrease master width factor"}),

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
            {group = "Launcher", description = "Open system menu"}),
        Key(M,  "t",     function () awful.spawn("launcher") end,
            {group = "Launcher", description = "Open ulauncher"})
    )

    for i = 1, 9 do
        root_keys = gears.table.join(root_keys,
            tag_numkey(i, M,     function(t) t:view_only() end),
            tag_numkey(i, CM,    function(t) awful.tag.viewtoggle(t) end),
            client_numkey(i, SM, function(t) client.focus:move_to_tag(t) end)
        )
    end

    self.client_keys = gears.table.join(
        Key(M,  "f",     function(c) c.fullscreen = not c.fullscreen c:raise() end,
            {group = "Windows", description = "Toggle fullscreen"}),
        Key(SM, "q",     function(c) c:kill() end,
            {group = "Windows", description = "Close"}),
        Key(M,  "m",     function(c) c.maximized = not c.maximized c:raise() end ,
            {group = "Windows", description = "(Un)Maximize"}),
        Key(M,  "b",     awful.client.floating.toggle,
            {group = "Windows", description = "Toggle floating"}),
        Key({}, "Print", screenshot_full,
            {group = "Screenshot", description = "Screenshot full screen"}),
        Key(S, "Print",  screenshot_select,
            {group = "Screenshot", description = "Screenshot selected rect"}),
        Key(C, "Print",  screenshot_focused,
            {group = "Screenshot", description = "Screenshot focused window"})
    )

    self.client_buttons = gears.table.join(
        Button({}, 1, function (c) client.focus = c; c:raise() end),
        Button(M,  1, awful.mouse.client.move),
        Button(M,  3, awful.mouse.client.resize)
    )

    root.keys(root_keys)
end

return keys
