local awful = require("awful")
local gears = require("gears")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local taglist = {}

-- Module functions
--------------------------------------------------------------------------------
function taglist:widget(screen)
	local widget = awful.widget.taglist({
		screen = screen,
		filter = awful.widget.taglist.filter.all,
		buttons = gears.table.join(
			awful.button({ }, 1, function(t) t:view_only() end),
			awful.button({ }, 3, awful.tag.viewtoggle))
	})

	return widget
end

-- Constructor
--------------------------------------------------------------------------------
function taglist:init(args)
	args = args or {}
end

return taglist
