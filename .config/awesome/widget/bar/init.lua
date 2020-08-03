local awful = require("awful")
local wibox = require("wibox")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local bar = {}

-- Module functions
--------------------------------------------------------------------------------
function bar:create(args)
	args = args or {}

	args.left.layout = wibox.layout.fixed.horizontal
	args.right.layout = wibox.layout.fixed.horizontal

	local widget = wibox.widget {
        layout = wibox.layout.align.horizontal,
        args.left,
        args.center,
        args.right,
	}

	screen[self.bar_id] = awful.wibar {
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
