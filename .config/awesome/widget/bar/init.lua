local awful = require("awful")
local wibox = require("wibox")
local beautiful = require('beautiful')

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local bar = {}

-- Module functions
--------------------------------------------------------------------------------
function bar:create(args)
	args = args or {}

	args.left.layout = wibox.layout.fixed.horizontal
	args.right.layout = wibox.layout.fixed.horizontal
	args.right.spacing = beautiful.systray_widget_spacing;

	local widget = wibox.widget {
		layout = wibox.layout.align.horizontal,
		args.left,
		args.center,
		wibox.widget {
			layout = wibox.container.margin,
			right = beautiful.systray_widget_spacing,
			args.right
		}
	}

	args.screen[self.bar_id] = awful.wibar {
		position = "top",
		screen = args.screen,
		widget = widget
	}
end

-- Constructor
--------------------------------------------------------------------------------
function bar:init(args)
	args = args or {}

	self.bar_id = args.bar_id or "bar"
end

return bar
