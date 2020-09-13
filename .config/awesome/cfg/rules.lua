local awful =require("awful")
local beautiful = require("beautiful")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local rules = {}

-- Constructor
--------------------------------------------------------------------------------
function rules:init(args)
    args = args or {}
    local client_keys = args.client_keys or {}
    local client_buttons = args.client_buttons or {}

    local base_properties = {
        -- from https://awesomewm.org/doc/api/sample%20files/rc.lua.html
        border_width = beautiful.border_width,
        border_color = beautiful.border_normal,
        focus = awful.client.focus.filter,
        raise = true,
        keys = client_keys,
        buttons = client_buttons,
        screen = awful.screen.preferred,
        placement = awful.placement.no_overlap+awful.placement.no_offscreen,
        -- Remove gaps
        size_hints_honor = false
    }

    floating_any = {
        class = {
          "MPlayer",
          "open3d",
          "Pyorgcalendar",
          "pinentry",
          "gimp",
          "RTGE",
          "terra",
          "TERRA",
        },
        type = { "dialog" }
    }

    awful.rules.rules = {
        -- All clients will match this rule.
        {
            rule = { },
            properties = base_properties,
            -- New client always open as slave, put it at the end of other windows.
            callback = awful.client.setslave
        },

        -- Floating clients.
        {
            rule_any = floating_any,
            properties = { floating = true }
        },

        -- Application specific
        {
            rule = { class = "Firefox" },
            properties = { tag = "web" }
        },
        {
            rule = { class = "Subl3" },
            properties = { tag = "doc" }
        },
        {
            rule = { class = "Doublecmd" },
            properties = { tag = "cmdr" }
        }
    }
end

return rules
