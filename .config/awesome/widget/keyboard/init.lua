local wibox = require("wibox")
local awful = require('awful')
local gdebug = require("gears.debug")
local beautiful = require('beautiful')
local awesome = awesome

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local keyboard = {}

-- Module functions
--------------------------------------------------------------------------------
local function update_text(self)
	self._layout_group_ind = awesome.xkb_get_layout_group()

	local text = ""
	if #self._layouts > 0 then
		local name = self._layouts[self._layout_group_ind+1]
		if name then
			text = name
		end
	end

	self.parent:set_text(text)
end

local function update_layouts(self)
	self._layouts = {}

	local layouts = awful.widget.keyboardlayout.get_groups_from_group_names(awesome.xkb_get_group_names())
	if layouts == nil or layouts[1] == nil then
		gdebug.print_error("Failed to get list of keyboard groups")
		return
	end

	if #layouts == 1 then
		layouts[1].group_idx = 1
	end

	for _, v in ipairs(layouts) do
		self._layouts[v.group_idx] = " [" .. v.file:upper() .. "] "
	end

	update_text(self)
end

local function next_layout(self)
	self._layout_group_ind = (self._layout_group_ind + 1) % (#self._layouts + 1)
	awesome.xkb_set_layout_group(self._layout_group_ind)
end

-- Constructor
--------------------------------------------------------------------------------
function keyboard.new(args)
	args = args or {}

	local parent = wibox.widget.textbox()
	parent.font = beautiful.tasklist_widget_font

	local self = wibox.widget.base.make_widget(parent, nil, {enable_properties=true})
	self.parent = parent

	update_layouts(self)

	awesome.connect_signal("xkb::map_changed", function ()
		update_layouts(self)
	end)

	awesome.connect_signal("xkb::group_changed", function ()
		update_text(self)
	end)

	self.buttons = {
		awful.button({ }, 1, function ()
			next_layout(self)
		end)
	}

	return self
end

return keyboard
