local string = string

local awful = require("awful")
local gears = require("gears")

local wibox = require("wibox")
local naughty = require("naughty")
local beautiful = require('beautiful')

local pulse = require("pulseaudio_dbus")

local icon = {
  high = beautiful.icon_theme .. "/scalable/status/audio-volume-high-symbolic.svg",
  med = beautiful.icon_theme .. "/scalable/status/audio-volume-medium-symbolic.svg",
  low = beautiful.icon_theme .. "/scalable/status/audio-volume-low-symbolic.svg",
  muted = beautiful.icon_theme .. "/scalable/status/audio-volume-muted-symbolic.svg",
}

local widget = wibox.widget {
  resize = true,
  widget = wibox.widget.imagebox
}

widget.tooltip = awful.tooltip({ objects = { widget },})

function widget:update_appearance(v)
  local i, msg

  if v == "Muted" then
	msg = v
	i = icon.muted
  else
	v = v == "Unmuted" and self.sink:get_volume_percent()[1] or tonumber(v)
	msg = string.format("%d%%", v)
	if v <= 33 then
	  i = icon.low
	elseif v <= 66 then
	  i = icon.med
	else
	  i = icon.high
	end
  end

  self.image = i
  self.tooltip:set_text(msg)

end

function widget:notify(v)
  local msg = tonumber(v) and string.format("%d%%", v) or v

  if self.notification then
	naughty.destroy(self.notification, naughty.notificationClosedReason.dismissedByCommand)
  end

  self.notification = naughty.notify(
	{
	  text=msg,
	  timeout=self.notification_timeout_seconds
	}
  )

end

function widget:update_sink(object_path)
  self.sink = pulse.get_device(self.connection, object_path)
end

function widget:update_sources(sources)
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

function widget.volume_up()
  if not widget.sink:is_muted() then
	widget.sink:volume_up()
  end
end

function widget.volume_down()
  if not widget.sink:is_muted() then
	widget.sink:volume_down()
  end
end

function widget.toggle_muted()
  widget.sink:toggle_muted()
end

function widget.volume_up_mic()
  if widget.source and not widget.source:is_muted() then
	widget.source:volume_up()
  end
end

function widget.volume_down_mic()
  if widget.source and not widget.source:is_muted() then
	widget.source:volume_down()
  end
end

function widget.toggle_muted_mic()
  if widget.source then
	widget.source:toggle_muted()
  end
end

widget:buttons(gears.table.join(
				 awful.button({ }, 1, widget.toggle_muted),
				 awful.button({ }, 3, function () awful.spawn(widget.mixer) end),
				 awful.button({ }, 4, widget.volume_up),
				 awful.button({ }, 5, widget.volume_down)))

function widget:connect_device(device)
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

function widget:init()
  local status, address = pcall(pulse.get_address)
  if not status then
	naughty.notify({title="Error while loading the PulseAudio widget",
					text=address,
					preset=naughty.config.presets.critical})
	return self
  end

  self.mixer = "pavucontrol"
  self.notification_timeout_seconds = 1

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

  self.__index = self

  return self
end

return widget:init()
