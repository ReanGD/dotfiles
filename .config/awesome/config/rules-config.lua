local awful =require("awful")
local beautiful = require("beautiful")

local rules = {}


-- Build rule table
-----------------------------------------------------------------------------------------------------------------------
function rules:init(hotkeys)
    local base_properties = {
        border_width = beautiful.border_width,
        border_color = beautiful.border_normal,
        focus = awful.client.focus.filter,
        raise = true,
        keys = hotkeys.client_keys,
        buttons = hotkeys.client_buttons,
        screen = awful.screen.preferred,
        -- hotkeys fix
        size_hints_honor = false,
        placement = awful.placement.no_overlap+awful.placement.no_offscreen
    }

    floating_any = {
        class = {
          "MPlayer",
          "open3d",
          "Pyorgcalendar",
          "pinentry",
          "gimp"
        },
        type = { "dialog" }
    }

    awful.rules.rules = {
        -- All clients will match this rule.
        {
            rule = { },
            properties = base_properties,
            -- new client always open as slave
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
        }, 
        {
            rule = { class = "Conky" },
            properties = {
                floating = true,
                sticky = true,
                ontop = false,
                focusable = false,
                size_hints = {"program_position", "program_size"}
            }
        }
    }
end

return rules
