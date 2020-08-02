local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local beautiful = require("beautiful")
local pulseaudio = require("widget.volume.pulseaudio")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local volume = {}

local math = math
local table = table
local string = string
local dpi = beautiful.xresources.apply_dpi

-- Interface functions
--------------------------------------------------------------------------------
function volume:volume_up(show_popup)
	pulseaudio:volume_up()
	self:on_show_popup(show_popup)
end

function volume:volume_down(show_popup)
	pulseaudio:volume_down()
	self:on_show_popup(show_popup)
end

function volume:toggle_mute(show_popup)
	pulseaudio:toggle_mute()
	self:on_show_popup(show_popup)
end

function volume:volume_up_mic(show_popup)
	pulseaudio:volume_up_mic()
	self:on_show_popup(show_popup)
end

function volume:volume_down_mic(show_popup)
	pulseaudio:volume_down_mic()
	self:on_show_popup(show_popup)
end

function volume:toggle_mute_mic(show_popup)
	pulseaudio:toggle_mute_mic()
	self:on_show_popup(show_popup)
end

-- Module functions
--------------------------------------------------------------------------------
function volume:_get_device_icon(device)
	local icons
	if device.is_output then
		icons = self.icons.output
	else
		icons = self.icons.input
	end
	if device.mute then
		return icons.muted
	else
		if device.volume_percent <= 33 then
			return icons.low
		elseif device.volume_percent <= 66 then
			return icons.medium
		else
			return icons.high
		end
	end
end

function volume:_create_popup_slider(device)
	local desc = wibox.widget {
		font = "Inter Medium 11",
		widget = wibox.widget.textbox
	}

	local icon = wibox.widget {
		resize = true,
		forced_height = dpi(32),
		forced_width = dpi(32),
		widget = wibox.widget.imagebox
	}

	icon:buttons(gears.table.join(
		awful.button({ }, 1, function ()
			device:toggle_mute()
		end)
	))

	local slider = wibox.widget {
		bar_shape           = gears.shape.rounded_rect,
		bar_height          = dpi(2),
		bar_color           = "#ffffff20",
		bar_active_color	= "#f2f2f2EE",
		handle_color        = "#ffffff",
		handle_shape        = gears.shape.circle,
		handle_width        = dpi(15),
		handle_border_color = "#00000012",
		handle_border_width = dpi(1),
		maximum				= 100,
		widget              = wibox.widget.slider,
		forced_height       = dpi(46),
		forced_width        = dpi(200),
	}

	slider:connect_signal(
		"property::value",
		function()
			if device.mute then
				return
			end
			local volume_percent = slider:get_value()
			if device.volume_percent ~= volume_percent then
				device:set_volume_percent(volume_percent)
			end
		end
	)

	local value = wibox.widget {
		font = "Inter Bold 12",
		widget = wibox.widget.textbox
	}

	local widget = wibox.widget {
		{
			{
				desc,
				bottom = dpi(4),
				widget = wibox.container.margin
			},
			{
				icon,
				slider,
				value,
				spacing = dpi(24),
				layout = wibox.layout.fixed.horizontal
			},
			layout = wibox.layout.fixed.vertical,
		},
		top = dpi(12),
		bottom = dpi(0),
		widget = wibox.container.margin
	}

	widget.desc = desc
	widget.icon = icon
	widget.slider = slider
	widget.value = value

	return widget
end

function volume:_create_tray_widget(device)
	local widget = wibox.widget {
		layout = wibox.layout.fixed.horizontal,
		{
			id = "icon_widget_id",
			widget = wibox.widget.imagebox,
		},
		{
			id = "text_widget_id",
			widget = wibox.widget.textbox
		}
	}

	widget.tooltip = awful.tooltip {
		objects = { widget },
		delay_show = 1,
		margin_leftright = dpi(8),
		margin_topbottom = dpi(8),
		mode = "outside",
	}

	menu_items = {}
	table.insert(menu_items,  {"toggle mute", function() device:toggle_mute() end } )
	table.insert(menu_items,  {"mixer", function() awful.spawn(self.mixer) end } )

	widget.menu = awful.menu(menu_items)

	widget:buttons(gears.table.join(
		awful.button({ }, 1, function ()
			self:on_show_popup(true)
		end),
		awful.button({ }, 3, function ()
			widget.menu:show()
		end),
		awful.button({ }, 4, function ()
			device:volume_up()
		end),
		awful.button({ }, 5, function ()
			device:volume_down()
		end)
	))

	return widget
end

function volume:_create_device_widgets(device)
	local need_update = false

	if not device.widget then
		need_update = true
		device.widget = self:_create_tray_widget(device)
	end

	if not device.popup_widget then
		need_update = true
		device.popup_widget = self:_create_popup_slider(device)
	end

	if need_update then
		self:on_device_changed(device)
	end
end

function volume:_create_popup_widget(enable)
	local widget = awful.popup {
		widget = wibox.widget {
			text = "Volume",
			font = "Inter Bold 12",
			align = "left",
			valign = "center",
			widget = wibox.widget.textbox
		},
		ontop = true,
		visible = false,
		type = "notification",
		bg = beautiful.transparent,
		shape = gears.shape.rectangle,
		offset = dpi(5),
		preferred_anchors = "back",
		preferred_positions = {"right", "left", "bottom", "top"}
	}

	widget.hide_timer = gears.timer {
		timeout = 2,
		single_shot = true,
		callback = function()
			if widget.enable_stop then
				widget.visible = false
			end
		end
	}

	widget:connect_signal(
		"mouse::enter",
		function()
			widget.enable_stop = false
		end
	)

	widget:connect_signal(
		"mouse::leave",
		function()
			widget.enable_stop = true
			widget.hide_timer:again()
		end
	)

	return widget
end

function volume:on_show_popup(enable)
	if not enable then
		return
	end

	if not self.popup_wiget.visible then
		local bar = awful.screen.focused()[self.bar_id]
		self.popup_wiget:move_next_to(bar)
		self.popup_wiget.enable_stop = true
		self.popup_wiget.visible = true
	end
	self.popup_wiget.hide_timer:again()
end

function volume:on_device_changed(device)
	local icon = self:_get_device_icon(device)
	local text, value_text, tooltip_text, popup_slider_value, popup_slider_max

	local tray_text = device.widget.text_widget_id
	local tray_icon = device.widget.icon_widget_id
	local popup_desc = device.popup_widget.desc
	local popup_icon = device.popup_widget.icon
	local popup_slider = device.popup_widget.slider
	local popup_value = device.popup_widget.value
	local tray_tooltip = device.widget.tooltip

	if device.mute then
		text = ""
		value_text = ""
		tooltip_text = "Muted"
		popup_slider_max = 1
		popup_slider_value = 0
	else
		text = string.format("%d", device.volume_percent)
		value_text = string.format("%d%%", device.volume_percent)
		tooltip_text = string.format("%s: %d%%", device.active_port_desc, device.volume_percent)
		popup_slider_max = 100
		popup_slider_value = device.volume_percent
	end

	tray_text.text = text
	popup_desc:set_text(device.active_port_desc)
	tray_tooltip:set_text(tooltip_text)
	popup_slider.maximum = popup_slider_max
	popup_slider:set_value(popup_slider_value)
	popup_value:set_text(value_text)
	tray_icon.image = icon
	popup_icon.image = icon
end

function volume:on_devices_changed(inputs, outputs)
	local tray_widgets = {
		spacing = dpi(5),
		layout = wibox.layout.fixed.horizontal
	}
	local popup_widgets = {
		layout = wibox.layout.fixed.vertical
	}

	for _, device in ipairs(inputs) do
		self:_create_device_widgets(device)
		table.insert(tray_widgets, device.widget)
		table.insert(popup_widgets, device.popup_widget)
	end

	for _, device in ipairs(outputs) do
		self:_create_device_widgets(device)
		table.insert(tray_widgets, device.widget)
		table.insert(popup_widgets, device.popup_widget)
	end

	self.widget:reset()
	self.widget:setup(tray_widgets)
	self.popup_wiget:setup({
		{
			popup_widgets,
			left = dpi(24),
			right = dpi(24),
			widget = wibox.container.margin
		},
		bg = beautiful.background,
		shape = gears.shape.rounded_rect,
		widget = wibox.container.background()
	})
end

-- Constructor
--------------------------------------------------------------------------------
function volume:init(args)
	args = args or {}

	self.bar_id = args.bar_id or "bar"
	self.mixer = args.mixer or "pavucontrol"

	local icons_dir = beautiful.icon_theme .. "/scalable/status/"
	self.icons = {
		output = {
			high    = icons_dir .. "audio-volume-high-symbolic.svg",
			medium  = icons_dir .. "audio-volume-medium-symbolic.svg",
			low     = icons_dir .. "audio-volume-low-symbolic.svg",
			muted   = icons_dir .. "audio-volume-muted-symbolic.svg",
		},
		input = {
			high   = icons_dir .. "microphone-sensitivity-high-symbolic.svg",
			medium = icons_dir .. "microphone-sensitivity-medium-symbolic.svg",
			low    = icons_dir .. "microphone-sensitivity-low-symbolic.svg",
			muted  = icons_dir .. "microphone-sensitivity-muted-symbolic.svg",
		},
	}

	self.widget = wibox.widget {
		layout = wibox.layout.fixed.horizontal
	}
	self.popup_wiget = self:_create_popup_widget()

	local on_device_changed = function (device) self:on_device_changed(device) end
	local on_devices_changed = function (inputs, outputs) self:on_devices_changed(inputs, outputs) end

	pulseaudio:init(on_device_changed, on_devices_changed)
end

return volume
