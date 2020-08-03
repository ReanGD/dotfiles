local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local beautiful = require("beautiful")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local clock = {}

local dpi = beautiful.xresources.apply_dpi

-- Module functions
--------------------------------------------------------------------------------
function clock:show_month_calendar()
	if self.tooltip.visible then
		self.tooltip.visible = false
	end
	if self.year_calendar.visible then
		self.year_calendar.visible = false
	end
	if not self.month_calendar.visible or self.month_calendar._calendar_clicked_on then
		self.month_calendar:call_calendar(0)
		self.month_calendar.visible = not self.month_calendar.visible
	end
	self.month_calendar._calendar_clicked_on = self.month_calendar.visible
end

function clock:show_year_calendar()
	if self.tooltip.visible then
		self.tooltip.visible = false
	end
	if self.month_calendar.visible then
		self.month_calendar.visible = false
	end
	if not self.year_calendar.visible or self.year_calendar._calendar_clicked_on then
		self.year_calendar:call_calendar(0)
		self.year_calendar.visible = not self.year_calendar.visible
	end
	self.year_calendar._calendar_clicked_on = self.year_calendar.visible
end

-- Constructor
--------------------------------------------------------------------------------
function clock:init(args)
	args = args or {}

	local format = args.format or "%a, %d %H:%M "
	local refresh_sec = args.refresh or 60

	self.widget = wibox.widget.textclock(format, refresh_sec)

	self.tooltip = awful.tooltip {
		objects = { self.widget },
		delay_show = 1,
		margin_leftright = dpi(8),
		margin_topbottom = dpi(8),
		mode = "outside",
		timer_function = function()
			return os.date("%d %B, %A <b>%d.%m.%Y</b>")
		end,
	}

	self.month_calendar = awful.widget.calendar_popup.month({
		position = "tr",
		spacing = dpi(5),
		margin = dpi(5),
		week_numbers = false,
		start_sunday = false,
		long_weekdays = false,
	})

	self.year_calendar = awful.widget.calendar_popup.year({
		position = "tr",
		spacing = dpi(5),
		margin = dpi(5),
		week_numbers = false,
		start_sunday = false,
		long_weekdays = false,
	})

	self.widget:buttons(gears.table.join(
		awful.button({ }, 1, function ()
			self:show_month_calendar()
		end),
		awful.button({ }, 3, function ()
			self:show_year_calendar()
		end)
	))
end

return clock
