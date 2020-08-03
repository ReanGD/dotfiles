local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local layoutbox = {}

-- Module functions
--------------------------------------------------------------------------------
function layoutbox:widget(screen)
	local widget = awful.widget.layoutbox(screen)
	widget:buttons(gears.table.join(
		awful.button({ }, 1, function () awful.layout.inc(1) end),
		awful.button({ }, 3, function () layoutbox:show_menu() end)
	))

	return widget
end

function layoutbox:_layout_menu_create()
	local form = awful.widget.layoutlist {
		base_layout = wibox.widget {
			spacing         = 5,
			forced_num_cols = 3,
			layout          = wibox.layout.grid.vertical,
		},

		widget_template = {
			{
				{
					id            = "icon_role",
					forced_height = 22,
					forced_width  = 22,
					widget        = wibox.widget.imagebox,
				},
				margins = 4,
				widget  = wibox.container.margin,
			},
			id              = "background_role",
			forced_width    = 48,
			forced_height   = 48,
			shape           = gears.shape.rounded_rect,
			widget          = wibox.container.background,
		},
	}

	local menu = awful.popup {
		widget = wibox.widget {
			form,
			margins = 4,
			widget  = wibox.container.margin,
		},
		border_color = beautiful.border_color,
		border_width = beautiful.border_width,
		placement    = awful.placement.centered,
		ontop        = true,
		visible      = false,
		shape        = gears.shape.rounded_rect
	}


	menu.form = form

	local M = { self.modkey }
	menu.keygrabber = awful.keygrabber {
		start_callback = function() menu.visible = true  end,
		stop_callback  = function() menu.visible = false end,
		export_keybindings = false,
		stop_key = { "Escape", "Super_L", "Super_R" },
		keybindings = {
			{ M, "j", function() self:layout_menu_focus_switch("left") end,
				{group = "Layout menu", description = "Go to left item"}},
			{ M, "l", function() self:layout_menu_focus_switch("right") end,
				{group = "Layout menu", description = "Go to right item"}},
			{ M, "i", function() self:layout_menu_focus_switch("up") end,
				{group = "Layout menu", description = "Go to up item"}},
			{ M, "k", function() self:layout_menu_focus_switch("down") end,
				{group = "Layout menu", description = "Go to down item"}},
		}
	}

	return menu
end

function layoutbox:layout_menu_focus_switch(dir)
	local step = 1
	if dir == "left" or dir == "up" then
		step = -1
	end
	local layout, _ = gears.table.cycle_value(self.layout_menu.form.layouts, self.layout_menu.form.current_layout, step)
	awful.layout.set(layout)
end

function layoutbox:layout_menu_show()
	self.layout_menu.keygrabber:start()
end

-- Constructor
--------------------------------------------------------------------------------
function layoutbox:init(args)
	args = args or {}

	self.modkey = args.modkey or "Mod4"
	self.layout_menu = self:_layout_menu_create()
end

return layoutbox
