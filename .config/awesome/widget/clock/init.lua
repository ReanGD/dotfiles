local wibox = require("wibox")
local awful = require('awful')
local gears = require('gears')
local beautiful = require('beautiful')
local abutton = require("awful.button")
local dpi = beautiful.xresources.apply_dpi

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local clock = {}

-- Constructor
--------------------------------------------------------------------------------
function clock:create(args)
	args = args or {}

	local format = args.format or "%a, %d %H:%M "
	local refresh_sec = args.refresh or 60
	clock_widget = wibox.widget.textclock(format, refresh_sec)
	clock_widget.font = beautiful.tasklist_widget_font

	local tooltip = awful.tooltip{
		objects = { clock_widget },
		delay_show = 1,
		margin_leftright = dpi(8),
		margin_topbottom = dpi(8),
		mode = "outside",
		timer_function = function()
			return os.date("%d %B, %A <b>%d.%m.%Y</b>")
		end,
	}

	local month_calendar = awful.widget.calendar_popup.month({
		position = "tr",
		font = beautiful.calendar_font,
		spacing = dpi(5),
		margin = dpi(5),
		week_numbers = false,
		start_sunday = false,
		long_weekdays = false,
	})

	local year_calendar = awful.widget.calendar_popup.year({
		position = "tr",
		font = beautiful.calendar_font,
		spacing = dpi(5),
		margin = dpi(5),
		week_numbers = false,
		start_sunday = false,
		long_weekdays = false,
	})

	clock_widget:buttons(gears.table.join(
		abutton({ }, 1, function ()
			if tooltip.visible then
				tooltip.visible = false
			end
			if year_calendar.visible then
				year_calendar.visible = false
			end
			if not month_calendar.visible or month_calendar._calendar_clicked_on then
				month_calendar:call_calendar(0)
				month_calendar.visible = not month_calendar.visible
			end
			month_calendar._calendar_clicked_on = month_calendar.visible
		end),
		abutton({ }, 3, function ()
			if tooltip.visible then
				tooltip.visible = false
			end
			if month_calendar.visible then
				month_calendar.visible = false
			end
			if not year_calendar.visible or year_calendar._calendar_clicked_on then
				year_calendar:call_calendar(0)
				year_calendar.visible = not year_calendar.visible
			end
			year_calendar._calendar_clicked_on = year_calendar.visible
		end)
	))

	return clock_widget
end

return clock
