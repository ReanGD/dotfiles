local awesome = awesome
local setmetatable = setmetatable

local textbox = require("wibox.widget.textbox")
local keyboardlayout = require("awful.widget.keyboardlayout")
local widget_base = require("wibox.widget.base")
local gdebug = require("gears.debug")

local keyboard = { mt = {} }


local function update_status(self)
    local text = ""
    if (#self._layout > 0) then
        local current = awesome.xkb_get_layout_group();
        text = self._layout[current]
    end

    self.widget:set_text(text)
end

function keyboard.new()
    local widget = textbox()
    local self = widget_base.make_widget(widget)

    self.widget = widget
    
    self._layout = {}
    local layouts = keyboardlayout.get_groups_from_group_names(awesome.xkb_get_group_names())
    if layouts == nil or layouts[1] == nil then
        gdebug.print_error("Failed to get list of keyboard groups")
        return
    end
    if #layouts == 1 then
        layouts[1].group_idx = 0
    end
    for _, v in ipairs(layouts) do
        self._layout[v.group_idx - 1] = " [" .. v.file:upper() .. "] "
    end

    update_status(self)
    awesome.connect_signal("xkb::group_changed", function () update_status(self) end)

    return self
end

function keyboard.mt:__call(...)
    return keyboard.new(...)
end

return setmetatable(keyboard, keyboard.mt)
