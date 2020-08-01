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
	-- if show_popup then
	-- 	awesome.emit_signal('module::volume_osd:show', true)
	-- end
end

function volume:volume_down(show_popup)
	pulseaudio:volume_down()
	-- if show_popup then
	-- 	awesome.emit_signal('module::volume_osd:show', true)
	-- end
end

function volume:toggle_mute(show_popup)
	pulseaudio:toggle_mute()
	-- if show_popup then
	-- 	awesome.emit_signal('module::volume_osd:show', true)
	-- end
end

function volume:volume_up_mic(show_popup)
	pulseaudio:volume_up_mic()
	-- if show_popup then
	-- 	awesome.emit_signal('module::volume_osd:show', true)
	-- end
end

function volume:volume_down_mic(show_popup)
	pulseaudio:volume_down_mic()
	-- if show_popup then
	-- 	awesome.emit_signal('module::volume_osd:show', true)
	-- end
end

function volume:toggle_mute_mic(show_popup)
	pulseaudio:toggle_mute_mic()
	-- if show_popup then
	-- 	awesome.emit_signal('module::volume_osd:show', true)
	-- end
end

-- Utils functions
--------------------------------------------------------------------------------
function volume:get_volume_percent(volume, base_volume)
	local volume = volume or self.sink:get_volume()[1]
	local base_volume = base_volume or self.sink.BaseVolume

	return math.max(0, math.ceil(volume * 100.0 / base_volume - 0.5))
end

-- Module functions
--------------------------------------------------------------------------------
function volume:update_widget(is_muted, volume_percent)
	local widget_icon, widget_text, tooltip_text, popup_value_text, popup_slider_max
	local is_muted = is_muted or self.sink:is_muted()

	if is_muted then
		widget_text = ""
		tooltip_text = "Muted"
		popup_value_text = "Muted"
		widget_icon = self.icons.muted
		popup_slider_max = 1
		self.volume_percent = 0
	else
		self.volume_percent = volume_percent or self:get_volume_percent()
		widget_text = string.format("%d", self.volume_percent)
		tooltip_text = string.format("%d%%", self.volume_percent)
		popup_value_text = string.format("%d%%", self.volume_percent)
		popup_slider_max = 100

		if self.volume_percent <= 33 then
			widget_icon = self.icons.low
		elseif self.volume_percent <= 66 then
			widget_icon = self.icons.medium
		else
			widget_icon = self.icons.high
		end
	end

	self.widget_text.text = widget_text
	self.widget_image.image = widget_icon
	self.widget_popup_slider.maximum = popup_slider_max
	self.widget_popup_slider:set_value(self.volume_percent)
	self.widget_popup_value.text = popup_value_text
	self.widget_popup_image.image = widget_icon
	self.tooltip:set_text(tooltip_text)
end

function volume:notify(v)
	local msg = tonumber(v) and string.format("%d%%", v) or v

	if self.notification then
		naughty.destroy(self.notification, naughty.notificationClosedReason.dismissedByCommand)
	end

	self.notification = naughty.notify({
		text = msg,
		timeout = self.notification_timeout_seconds
	})
end

function volume:popup_create_header()
	local popup_header = wibox.widget {
		text = "Volume",
		font = "Inter Bold 12",
		align = "left",
		valign = "center",
		widget = wibox.widget.textbox
	}

	self.widget_popup_value = wibox.widget {
		text = "0%",
		font = "Inter Bold 12",
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox
	}

	return wibox.widget {
		popup_header,
		nil,
		self.widget_popup_value,
		expand = "none",
		forced_height = dpi(48),
		layout = wibox.layout.align.horizontal
	}
end

function volume:popup_create_slider()
	local slider_image = wibox.widget {
		{
			id = "image_widget_id",
			resize = true,
			widget = wibox.widget.imagebox
		},
		top = dpi(12),
		bottom = dpi(12),
		widget = wibox.container.margin
	}

	local slider = wibox.widget {
		nil,
		{
			id 					= "slider_widget_id",
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
		},
		nil,
		expand = "none",
		layout = wibox.layout.align.vertical
	}

	self.widget_popup_image = slider_image.image_widget_id
	self.widget_popup_slider = slider.slider_widget_id

	slider_image:buttons(gears.table.join(
		awful.button({ }, 1, function ()
			self:toggle_muted(false)
		end)
	))

	self.widget_popup_slider:connect_signal(
		"property::value",
		function()
			local volume_percent = tonumber(self.widget_popup_slider:get_value())
			if self.volume_percent == volume_percent then
				return
			end

			if not self.sink:is_muted() then
				local volume = volume_percent * self.sink.BaseVolume / 100
				local volumes = self.sink:get_volume_percent()
				for i, v in ipairs(volumes) do
					volumes[i] = volume
				end

				self.sink:set_volume(volumes)
			end
		end
	)

	self.widget_popup_slider:connect_signal(
		"button::press",
		function()
			self.show_popup = true
		end
	)

	self.widget_popup_slider:connect_signal(
		"mouse::enter",
		function()
			self.show_popup = true
		end
	)

	return wibox.widget {
		slider_image,
		slider,
		spacing = dpi(24),
		layout = wibox.layout.fixed.horizontal
	}
end

function volume:popup_create_volume_dialog()
	local header = self:popup_create_header()
	local slider = self:popup_create_slider()

	return wibox.widget {
		{
			{
				header,
				slider,
				layout = wibox.layout.fixed.vertical
			},
			left = dpi(24),
			right = dpi(24),
			widget = wibox.container.margin
		},
		bg = beautiful.background,
		shape = gears.shape.rounded_rect,
		widget = wibox.container.background()
	}
end

function volume:init_popup()
	self.show_popup = false

	-- Create the box
	local osd_height = dpi(100)
	local osd_width = dpi(300)
	local osd_margin = dpi(10)

	self.volume_osd_overlay = awful.popup {
		widget = self:popup_create_volume_dialog(),
		ontop = true,
		visible = false,
		type = "notification",
		height = osd_height,
		width = osd_width,
		maximum_height = osd_height,
		maximum_width = osd_width,
		offset = dpi(5),
		shape = gears.shape.rectangle,
		bg = beautiful.transparent,
		preferred_anchors = "middle",
		preferred_positions = {"right", "left", "top", "bottom"}
	}

	local hide_osd = gears.timer {
		timeout = 2,
		autostart = true,
		callback  = function()
			self.volume_osd_overlay.visible = false
			self.show_popup = false
		end
	}

	local timer_rerun = function()
		if hide_osd.started then
			hide_osd:again()
		else
			hide_osd:start()
		end
	end

	-- Reset timer on mouse hover
	self.volume_osd_overlay:connect_signal(
		'mouse::enter',
		function()
			-- s.show_popup = true
			timer_rerun()
		end
	)

	local placement_placer = function()

		local focused = awful.screen.focused()

		local right_panel = focused.right_panel
		local left_panel = focused.left_panel
		local volume_osd = focused.volume_osd_overlay

		if right_panel and left_panel then
			if right_panel.visible then
				awful.placement.bottom_left(
					focused.volume_osd_overlay,
					{
						margins = {
							left = osd_margin,
							right = 0,
							top = 0,
							bottom = osd_margin
						},
						honor_workarea = true
					}
				)
				return
			end
		end

		if right_panel then
			if right_panel.visible then
				awful.placement.bottom_left(
					focused.volume_osd_overlay,
					{
						margins = {
							left = osd_margin,
							right = 0,
							top = 0,
							bottom = osd_margin
						},
						honor_workarea = true
					}
				)
				return
			end
		end

		awful.placement.bottom_right(
			focused.volume_osd_overlay,
			{
				margins = {
					left = 0,
					right = osd_margin,
					top = 0,
					bottom = osd_margin,
				},
				honor_workarea = true
			}
		)
	end

	awesome.connect_signal(
		'module::volume_osd:show',
		function(bool)
			placement_placer()
			self.volume_osd_overlay.visible = bool
			if bool then
				timer_rerun()
				awesome.emit_signal(
					'module::brightness_osd:show',
					false
				)
			else
				if hide_osd.started then
					hide_osd:stop()
				end
			end
		end
	)
end

function volume:on_item_changed(device)
	local tray_text = device.widget.text_widget_id
	local tray_icon = device.widget.icon_widget_id
	local tray_tooltip = device.tooltip

	if device.mute then
		tray_text.text = ""
		tray_icon.image = self.icons.muted
		tray_tooltip:set_text("Muted")
	else
		local icon
		if device.volume_percent <= 33 then
			icon = self.icons.low
		elseif device.volume_percent <= 66 then
			icon = self.icons.medium
		else
			icon = self.icons.high
		end
		local tooltip_text = string.format("%s: %d%%", device.active_port_desc, device.volume_percent)

		tray_text.text = string.format("%d", device.volume_percent)
		tray_icon.image = icon
		tray_tooltip:set_text(tooltip_text)
	end
end

function volume:on_outputs_changed(outputs)
	local widgets = {
		layout = wibox.layout.fixed.horizontal
	}

	for _, device in ipairs(outputs) do
		if not device.widget then
			device.widget = wibox.widget {
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

			device.tooltip = awful.tooltip {
				objects = { device.widget },
				delay_show = 1,
				margin_leftright = dpi(8),
				margin_topbottom = dpi(8),
				mode = "outside",
			}

			self:on_item_changed(device)
		end
		table.insert(widgets, device.widget)
	end

	self.widget:setup(widgets)
end

function volume:on_inputs_changed(inputs)
end

-- Constructor
--------------------------------------------------------------------------------
function volume:init(args)
	args = args or {}

	local mixer = args.mixer or "pavucontrol"

	self.icons = {
		high = beautiful.icon_theme .. "/scalable/status/audio-volume-high-symbolic.svg",
		medium = beautiful.icon_theme .. "/scalable/status/audio-volume-medium-symbolic.svg",
		low = beautiful.icon_theme .. "/scalable/status/audio-volume-low-symbolic.svg",
		muted = beautiful.icon_theme .. "/scalable/status/audio-volume-muted-symbolic.svg",
	}

	self.notification_timeout_seconds = 1

	-- self.widget:buttons(gears.table.join(
	-- 	awful.button({ }, 1, function ()
	-- 		self:toggle_muted(false)
	-- 	end),
	-- 	awful.button({ }, 3, function ()
	-- 		awful.spawn(mixer)
	-- 	end),
	-- 	awful.button({ }, 4, function ()
	-- 		self:volume_up(false)
	-- 	end),
	-- 	awful.button({ }, 5, function ()
	-- 		self:volume_down(false)
	-- 	end)
	-- ))

	-- self:init_popup()
	self.widget = wibox.widget{
		layout = wibox.layout.fixed.horizontal
	}

	local on_outputs_changed = function (outputs) self:on_outputs_changed(outputs) end
	local on_inputs_changed = function (inputs) self:on_inputs_changed(inputs) end
	local on_item_changed = function (device) self:on_item_changed(device) end

	pulseaudio:init(on_outputs_changed, on_inputs_changed, on_item_changed)
end

return volume
