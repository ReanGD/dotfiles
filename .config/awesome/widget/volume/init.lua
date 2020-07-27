local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local beautiful = require("beautiful")
local pulse = require("pulseaudio_dbus")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local volume = {}

local string = string
local dpi = beautiful.xresources.apply_dpi

-- Module functions
--------------------------------------------------------------------------------
function volume:update_appearance(v)
	local icon, msg

	if v == "Muted" then
		msg = v
		icon = self.icons.muted
	else
		v = v == "Unmuted" and self.sink:get_volume_percent()[1] or tonumber(v)
		msg = string.format("%d%%", v)

		if v <= 33 then
			icon = self.icons.low
		elseif v <= 66 then
			icon = self.icons.medium
		else
			icon = self.icons.high
		end
	end

	self.widget.image = icon
	self.tooltip:set_text(msg)
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
				-- FIXME: BaseVolume for sources (i.e. microphones) won't give the correct percentage
				local v = math.ceil(tonumber(volume[1]) / this.BaseVolume * 100)
				if this.object_path == self.sink.object_path then
					self:update_appearance(v)
					self:notify(v)
				end
			end,
			"VolumeUpdated"
		)
	end

	if device.signals.MuteUpdated then
		device:connect_signal(
			function (this, is_mute)
				local m = is_mute and "Muted" or "Unmuted"
				if this.object_path == self.sink.object_path then
					self:update_appearance(m)
					self:notify(m)
				end
			end,
			"MuteUpdated"
		)
	end
end

-- Constructor
--------------------------------------------------------------------------------
function volume:init(args)
	args = args or {}

	local status, address = pcall(pulse.get_address)
	if not status then
		naughty.notify({
			title = "Error while loading widget 'volume'",
			text = address,
			preset = naughty.config.presets.critical
		})

		return self
	end

	self.connection = pulse.get_connection(address)
	self.core = pulse.get_core(self.connection)

	self.icons = {
		high = beautiful.icon_theme .. "/scalable/status/audio-volume-high-symbolic.svg",
		medium = beautiful.icon_theme .. "/scalable/status/audio-volume-medium-symbolic.svg",
		low = beautiful.icon_theme .. "/scalable/status/audio-volume-low-symbolic.svg",
		muted = beautiful.icon_theme .. "/scalable/status/audio-volume-muted-symbolic.svg",
	}

	self.mixer = "pavucontrol"
	self.notification_timeout_seconds = 1

	self.widget = wibox.widget {
		resize = true,
		widget = wibox.widget.imagebox
	}

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
			awful.spawn(widget.mixer)
		end),
		awful.button({ }, 4, function ()
			self:volume_up()
		end),
		awful.button({ }, 5, function ()
			self:volume_down()
		end)
	))

	-- listen on ALL objects as sinks and sources may change
	self.core:ListenForSignal("org.PulseAudio.Core1.Device.VolumeUpdated", {})
	self.core:ListenForSignal("org.PulseAudio.Core1.Device.MuteUpdated", {})

	self.core:ListenForSignal("org.PulseAudio.Core1.NewSink", {self.core.object_path})
	self.core:connect_signal(
		function (_, newsink)
			self:update_sink(newsink)
			self:connect_device(self.sink)
			local volume = self.sink:is_muted() and "Muted" or self.sink:get_volume_percent()[1]
			self:update_appearance(volume)
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

	local volume = self.sink:is_muted() and "Muted" or self.sink:get_volume_percent()[1]
	self:update_appearance(volume)
end

return volume
