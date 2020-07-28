local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local beautiful = require("beautiful")
local pulse = require("pulseaudio_dbus")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local volume = {}

local math = math
local string = string
local dpi = beautiful.xresources.apply_dpi


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
	local widget_icon, widget_text, tooltip_text
	local is_muted = is_muted or self.sink:is_muted()

	if is_muted then
		widget_text = ""
		tooltip_text = "Muted"
		widget_icon = self.icons.muted
	else
		local volume_percent = volume_percent or self:get_volume_percent()
		widget_text = string.format("%d", volume_percent)
		tooltip_text = string.format("%d%%", volume_percent)

		if volume_percent <= 33 then
			widget_icon = self.icons.low
		elseif volume_percent <= 66 then
			widget_icon = self.icons.medium
		else
			widget_icon = self.icons.high
		end
	end

	self.widget_text.text = widget_text
	self.widget_image.image = widget_icon
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

function volume:update_sink(object_path)
	self.sink = pulse.get_device(self.connection, object_path)
end

function volume:update_sources(sources)
	for _, source_path in ipairs(sources) do
		local s = pulse.get_device(self.connection, source_path)
		if s.Name and not s.Name:match("%.monitor$") then
			self.source = s
			break
		else
			self.source = nil
		end
	end
end

function volume:volume_up()
	if not self.sink:is_muted() then
		self.sink:volume_up()
	end
end

function volume:volume_down()
	if not self.sink:is_muted() then
		self.sink:volume_down()
	end
end

function volume:toggle_muted()
	self.sink:toggle_muted()
end

function volume:volume_up_mic()
	if self.source and not self.source:is_muted() then
		self.source:volume_up()
	end
end

function volume:volume_down_mic()
	if self.source and not self.source:is_muted() then
		self.source:volume_down()
	end
end

function volume:toggle_muted_mic()
	if self.source then
		self.source:toggle_muted()
	end
end

function volume:connect_device(device)
	if not device then
		return
	end

	if device.signals.VolumeUpdated then
		device:connect_signal(
			function (this, volume)
				if this.object_path == self.sink.object_path then
					self:update_widget(false, self:get_volume_percent(tonumber(volume[1]), this.BaseVolume))
				end
			end,
			"VolumeUpdated"
		)
	end

	if device.signals.MuteUpdated then
		device:connect_signal(
			function (this, is_mute)
				if this.object_path == self.sink.object_path then
					self:update_widget(is_mute)
				end
			end,
			"MuteUpdated"
		)
	end
end

function volume:init_dbus(iteration)
	local status, address = pcall(pulse.get_address)
	if not status then
		if iteration == 30 then
			naughty.notify({
				title = "Error while loading widget 'volume'",
				text = address,
				preset = naughty.config.presets.critical
			})
		else
			gears.timer.start_new(1, function()
				self:init_dbus(iteration + 1)
				return false
			end)
		end

		return
	end

	self.connection = pulse.get_connection(address)
	self.core = pulse.get_core(self.connection)

	-- listen on ALL objects as sinks and sources may change
	self.core:ListenForSignal("org.PulseAudio.Core1.Device.VolumeUpdated", {})
	self.core:ListenForSignal("org.PulseAudio.Core1.Device.MuteUpdated", {})

	self.core:ListenForSignal("org.PulseAudio.Core1.NewSink", {self.core.object_path})
	self.core:connect_signal(
		function (_, newsink)
			self:update_sink(newsink)
			self:connect_device(self.sink)
			self:update_widget()
		end,
		"NewSink"
	)

	self.core:ListenForSignal("org.PulseAudio.Core1.NewSource", {self.core.object_path})
	self.core:connect_signal(
		function (_, newsource)
			self:update_sources({newsource})
			self:connect_device(self.source)
		end,
		"NewSource"
	)

	self:update_sources(self.core:get_sources())
	self:connect_device(self.source)

	local sink_path = assert(self.core:get_sinks()[1], "No sinks found")
	self:update_sink(sink_path)
	self:connect_device(self.sink)
	self:update_widget()
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

	self.widget = wibox.widget {
		layout = wibox.layout.fixed.horizontal,
		{
			id = "image",
			widget = wibox.widget.imagebox,
		},
		{
			id = "text",
			widget = wibox.widget.textbox,
			text = ""
		}
	}

	self.widget_image = self.widget:get_children_by_id("image")[1]
	self.widget_text = self.widget:get_children_by_id("text")[1]

	self.tooltip = awful.tooltip {
		objects = { self.widget },
		delay_show = 1,
		margin_leftright = dpi(8),
		margin_topbottom = dpi(8),
		mode = "outside",
	}

	self.widget:buttons(gears.table.join(
		awful.button({ }, 1, function ()
			self:toggle_muted()
		end),
		awful.button({ }, 3, function ()
			awful.spawn(mixer)
		end),
		awful.button({ }, 4, function ()
			self:volume_up()
		end),
		awful.button({ }, 5, function ()
			self:volume_down()
		end)
	))

	self:init_dbus(0)
end

return volume
