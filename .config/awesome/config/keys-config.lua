local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")

local volume = require("widget.volume")
local layoutbox = require("widget.layoutbox")
local screenshot = require("widget.screenshot")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local keys = {}

local table = gears.table

-- Local functions
--------------------------------------------------------------------------------
local function screen_focus_switch(dir)
    return function()
        awful.screen.focus_bydirection(dir)
	end
end

local function client_focus_switch(dir)
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

local function position_switch(dir)
    return function()
        awful.client.swap.bydirection(dir)
	end
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

-- Constructor
--------------------------------------------------------------------------------
function keys:init(args)
    args = args or {}
    local modkey = args.modkey or "Mod4"
    local terminal = args.terminal or "urxvt"

    local S = { "Shift" }
    local C = { "Control" }
    local M = { modkey }
    local SM = { modkey, "Shift" }
    local CM = { modkey, "Control" }
    local SCM = { modkey, "Shift", "Control" }
    local Key = awful.key
    local Button = awful.button

    local root_keys = table.join(
        -- Awesome
        Key(SCM, "r", awesome.restart,
            {group = "Awesome", description = "Reload awesome"}),
        Key(SCM, "q", awesome.quit,
            {group = "Awesome", description = "Quit awesome"}),
        Key(M, "F1",  hotkeys_popup.widget.show_help,
            {group = "Awesome", description = "Show help"}),

        -- Screen focus
        Key(M, "Left",  screen_focus_switch("left"),
            {group = "Screen focus", description = "Go to left screen"}),
        Key(M, "Right", screen_focus_switch("right"),
            {group = "Screen focus", description = "Go to right screen"}),
        Key(M, "Up",    screen_focus_switch("up"),
            {group = "Screen focus", description = "Go to up screen"}),
        Key(M, "Down",  screen_focus_switch("down"),
            {group = "Screen focus", description = "Go to down screen"}),

        -- Window focus
        Key(M,  "j", client_focus_switch("left"),
            {group = "Window focus", description = "Go to left window"}),
        Key(M,  "l", client_focus_switch("right"),
            {group = "Window focus", description = "Go to right window"}),
        Key(M,  "i", client_focus_switch("up"),
            {group = "Window focus", description = "Go to up window"}),
        Key(M,  "k", client_focus_switch("down"),
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

        -- Windows size
        Key(CM, "l",  function () awful.tag.incmwfact( 0.05) end,
            {group = "Window size", description = "Increase window width"}),
        Key(CM, "j",  function () awful.tag.incmwfact(-0.05) end,
            {group = "Window size", description = "Decrease window width"}),
        Key(CM, "i",  function () awful.client.incwfact(0.05) end,
            {group = "Window size", description = "Increase window height"}),
        Key(CM, "k",  function () awful.client.incwfact(-0.05) end,
            {group = "Window size", description = "Decrease window height"}),

        -- Audio
        Key({}, "XF86AudioRaiseVolume", function () volume:volume_up(true) end,
            {group = "Audio", description = "Increase volume up by 5%"}),
        Key(SM, "=", function () volume:volume_up(true) end,
            {group = "Audio", description = "Increase volume up by 5%"}),
        Key({}, "XF86AudioLowerVolume", function () volume:volume_down(true) end,
            {group = "Audio", description = "decrease volume up by 5%"}),
        Key(SM, "-", function () volume:volume_down(true) end,
            {group = "Audio", description = "decrease volume up by 5%"}),
        Key({}, "XF86AudioMute", function () volume:toggle_mute(true) end,
            {group = "Audio", description = "Toggle mute"}),
        Key(SM, "0", function () volume:toggle_mute(true) end,
            {group = "Audio", description = "Toggle mute"}),

        -- Screenshot
        Key({}, "Print", function () screenshot:screen() end,
            {group = "Screenshot", description = "Screenshot screen"}),
        Key(S, "Print",  function () screenshot:rect() end,
            {group = "Screenshot", description = "Screenshot selected rect"}),
        Key(C, "Print",  function () screenshot:focused_window() end,
            {group = "Screenshot", description = "Screenshot focused window"}),

        Key(M, "F3", function () layoutbox:layout_menu_show() end,
            {group = "Main", description = "Window control mode"}),

        -- Tags
        Key(M, "Tab",   awful.tag.history.restore,
            {group = "Tag", description = "Go back"}),

        -- Layout
        Key(M, "space", function () awful.layout.inc(1) end,
            {group = "Layout", description = "Select next"}),

        -- Launcher
        Key(M, "Return", function () awful.spawn(terminal) end,
            {group = "Launcher", description = "Run terminal"}),
        Key(M,  "r",     function () awful.spawn("rofi-interactive") end,
            {group = "Launcher", description = "Open program menu"}),
        Key(SM, "r",     function () awful.spawn("rofi-custom-menu") end,
            {group = "Launcher", description = "Open custom menu"}),
        Key(M,  "t",     function () awful.spawn("rofi-translate") end,
            {group = "Launcher", description = "Translate text"})
    )

    for i = 1, 9 do
        root_keys = table.join(root_keys,
            tag_numkey(i, M,     function(t) t:view_only() end),
            tag_numkey(i, CM,    function(t) awful.tag.viewtoggle(t) end),
            client_numkey(i, SM, function(t) client.focus:move_to_tag(t) end)
        )
    end

    self.client_keys = table.join(
        Key(M,  "f",     function(c) c.fullscreen = not c.fullscreen c:raise() end,
            {group = "Windows", description = "Toggle fullscreen"}),
        Key(SM, "q",     function(c) c:kill() end,
            {group = "Windows", description = "Close"}),
        Key(M,  "m",     function(c) c.maximized = not c.maximized c:raise() end ,
            {group = "Windows", description = "(Un)Maximize"}),
        Key(M,  "b",     awful.client.floating.toggle,
            {group = "Windows", description = "Toggle floating"})
    )

    self.client_buttons = table.join(
        Button({}, 1, function (c) client.focus = c; c:raise() end),
        Button(M,  1, awful.mouse.client.move),
        Button(M,  3, awful.mouse.client.resize)
    )

    root.keys(root_keys)
end

return keys
