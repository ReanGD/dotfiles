local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local keyboard = {}

local awesome = awesome

-- Module functions
--------------------------------------------------------------------------------
function keyboard:update_text()
	self._layout_group_ind = awesome.xkb_get_layout_group()

	local text = ""
	if #self._layouts > 0 then
		local name = self._layouts[self._layout_group_ind+1]
		if name then
			text = name
		end
	end

	self.widget:set_text(text)
end

function keyboard:update_layouts()
	self._layouts = {}

	local layouts = awful.widget.keyboardlayout.get_groups_from_group_names(awesome.xkb_get_group_names())
	if layouts == nil or layouts[1] == nil then
		naughty.notify({
			title = "Error while update layouts for widget 'keyboard'",
			text = "Failed to get list of keyboard groups",
			preset = naughty.config.presets.critical
		})
		return
	end

	if #layouts == 1 then
		layouts[1].group_idx = 1
	end

	for _, v in ipairs(layouts) do
		self._layouts[v.group_idx] = "[" .. v.file:upper() .. "]"
	end

	keyboard:update_text()
end

function keyboard:next_layout()
	self._layout_group_ind = (self._layout_group_ind + 1) % (#self._layouts + 1)
	awesome.xkb_set_layout_group(self._layout_group_ind)
end

-- Constructor
--------------------------------------------------------------------------------
function keyboard:init(args)
	args = args or {}

	self.widget = wibox.widget.textbox()

	self:update_layouts()

	awesome.connect_signal("xkb::map_changed", function ()
		self:update_layouts()
	end)

	awesome.connect_signal("xkb::group_changed", function ()
		self:update_text()
	end)

	self.widget:buttons(gears.table.join(
		awful.button({ }, 1, function ()
			self:next_layout()
		end)
	))
end

return keyboard
