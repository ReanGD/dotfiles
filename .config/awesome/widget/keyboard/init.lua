local wibox = require("wibox")
local awful = require('awful')
local gears = require('gears')
local beautiful = require('beautiful')
local abutton = require("awful.button")
local textbox = require("wibox.widget.textbox")
local widget_base = require("wibox.widget.base")
local kb = require("awful.widget.keyboardlayout")
local capi = {awesome = awesome}
local dpi = beautiful.xresources.apply_dpi

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local keyboard = {}

local function update_status (self)
    self._current = awesome.xkb_get_layout_group()
    local text = ""
    if #self._layout > 0 then
        -- Please note that the group number reported by xkb_get_layout_group
        -- is lower by one than the group numbers reported by xkb_get_group_names.
        local name = self._layout[self._current+1]
        if name then
            text = " " .. name .. " "
        end
    end
    self.widget:set_text(text)
end

local function update_layout(self)
    self._layout = {};
    local layouts = kb.get_groups_from_group_names(awesome.xkb_get_group_names())
    if layouts == nil or layouts[1] == nil then
        gdebug.print_error("Failed to get list of keyboard groups")
        return
    end
    if #layouts == 1 then
        layouts[1].group_idx = 1
    end
    for _, v in ipairs(layouts) do
        self._layout[v.group_idx] = "[" .. v.file:upper() .. "]"
    end
    update_status(self)
end

-- Constructor
--------------------------------------------------------------------------------
function keyboard:create(args)
	args = args or {}

    local tbox = textbox()
    local widget = widget_base.make_widget(tbox, nil, {enable_properties=true})

    widget.widget = tbox

    widget.next_layout = function()
        widget.set_layout((widget._current + 1) % (#widget._layout + 1))
    end

    widget.set_layout = function(group_number)
        if (0 > group_number) or (group_number > #widget._layout) then
            error("Invalid group number: " .. group_number ..
                    "expected number from 0 to " .. #widget._layout)
            return;
        end
        awesome.xkb_set_layout_group(group_number)
    end

    update_layout(widget)

    -- callback for processing layout changes
    capi.awesome.connect_signal("xkb::map_changed",
                                function () update_layout(widget) end)
    capi.awesome.connect_signal("xkb::group_changed",
                                function () update_status(widget) end);

    -- Mouse bindings
    widget.buttons = {
        button({ }, 1, widget.next_layout)
    }

    return widget

	-- return widget
end

return keyboard
