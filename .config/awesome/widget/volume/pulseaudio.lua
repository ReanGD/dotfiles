local lgi =  require("lgi")
local gears = require("gears")
local naughty = require("naughty")
local timer = require("std.timer")
local proxy = require("dbus_proxy")
local pulse = require("pulseaudio_dbus")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local pulseaudio = {}
pulseaudio.port = {}
pulseaudio.device = {}
pulseaudio.core = {}

local math = math
local table = table

-- Local functions
--------------------------------------------------------------------------------
local function add_interface(instance, interface)
	for ind, attribute in pairs(interface) do
		assert(instance[ind] == nil, "Cannot override attribute " .. ind)
		instance[ind] = attribute
	end
end

local function get_volume_percent(volume, base_volume)
	return math.max(0, math.ceil(volume * 100.0 / base_volume - 0.5))
end

-- pulseaudio.port functions
--------------------------------------------------------------------------------
function pulseaudio.port.create(connection, path)
	local self = proxy.Proxy:new({
		bus = connection,
		name=nil,
		path=path,
		-- see: https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/DevicePort/
		interface="org.PulseAudio.Core1.DevicePort"
	})

	add_interface(self, pulseaudio.port)

	return self
end

-- pulseaudio.device functions
--------------------------------------------------------------------------------
function pulseaudio.device.create(connection, path)
	local self = proxy.Proxy:new({
		bus = connection,
		name=nil,
		path=path,
		-- see: https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/Device/
		interface="org.PulseAudio.Core1.Device"
	})

	add_interface(self, pulseaudio.device)

	self.connection = connection
	self.mute = self:get_mute()
	self.volume_percent = self:get_volume_percent()
	local port_path = self:get_active_port()
	if port_path == "" then
		self.active_port_desc = "<unknown>"
	else
		self.active_port_desc = pulseaudio.port.create(self.connection, port_path).Description
	end

	if self.signals.MuteUpdated then
		self:connect_signal(
			function (this, is_mute)
				if this.object_path == self.object_path then
					self.muted = is_mute
				end
			end,
			"MuteUpdated"
		)
	end

	if self.signals.VolumeUpdated then
		self:connect_signal(
			function (this, volumes)
				if this.object_path == self.object_path then
					self.volume_percent = get_volume_percent(tonumber(volumes[1]), self.BaseVolume)
				end
			end,
			"VolumeUpdated"
		)
	end

	if self.signals.ActivePortUpdated then
		self:connect_signal(
			function (this, port_path)
				if this.object_path == self.object_path then
					self.active_port_desc = pulseaudio.port.create(self.connection, port_path).Description
				end
			end,
			"ActivePortUpdated"
		)
	end

	return self
end

function pulseaudio.device:get_volumes()
	return self:Get("org.PulseAudio.Core1.Device", "Volume")
end

function pulseaudio.device:set_volumes(value)
	self:Set("org.PulseAudio.Core1.Device", "Volume", lgi.GLib.Variant("au", value))
	self.Volume = {signature="au", value=value}
end

function pulseaudio.device:get_volume_percent()
	return get_volume_percent(self:get_volumes()[1], self.BaseVolume)
end

function pulseaudio.device:set_volume_percent(value)
	local volume = value * self.BaseVolume / 100
	local volumes = self:get_volumes()
	for i, v in ipairs(volumes) do
		volumes[i] = volume
	end
	self.set_volumes(volumes)
	self.volume_percent = value
end

function pulseaudio.device:get_mute()
	return self:Get("org.PulseAudio.Core1.Device", "Mute")
end

function pulseaudio.device:set_mute(value)
	self:Set("org.PulseAudio.Core1.Device", "Mute", lgi.GLib.Variant("b", value))
	self.Mute = {signature="b", value=value}
	self.mute = value
end

function pulseaudio.device:get_active_port()
	if #self.Ports == 0 then
		return ""
	else
		return self:Get("org.PulseAudio.Core1.Device", "ActivePort")
	end
end

-- pulseaudio.core functions
--------------------------------------------------------------------------------
function pulseaudio.core.create(connection)
	local self = proxy.Proxy:new({
		bus = connection,
		name = nil, -- nil, because bus is *not* a message bus.
		path = "/org/pulseaudio/core1",
		-- see https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/Core/
		interface = "org.PulseAudio.Core1"
	})

	add_interface(self, pulseaudio.core)

	self.outputs = {}
	self.inputs = {}
	self.connection = connection

	self:ListenForSignal("org.PulseAudio.Core1.Device.MuteUpdated", {})
	self:ListenForSignal("org.PulseAudio.Core1.Device.VolumeUpdated", {})
	self:ListenForSignal("org.PulseAudio.Core1.Device.ActivePortUpdated", {})
	self:ListenForSignal("org.PulseAudio.Core1.NewSink", { self.object_path })
	self:ListenForSignal("org.PulseAudio.Core1.SinkRemoved", { self.object_path })
	self:ListenForSignal("org.PulseAudio.Core1.NewSource", { self.object_path })
	self:ListenForSignal("org.PulseAudio.Core1.SourceRemoved", { self.object_path })

	self:connect_signal(
		function (_, sink_path)
			self:outputs_add(sink_path)
		end,
		"NewSink"
	)

	self:connect_signal(
		function (_, sink_path)
			self:outputs_remove(sink_path)
		end,
		"SinkRemoved"
	)

	self:connect_signal(
		function (_, source_path)
			self:inputs_add(source_path)
		end,
		"NewSource"
	)

	self:connect_signal(
		function (_, source_path)
			self:inputs_remove(source_path)
		end,
		"SourceRemoved"
	)

	for _, sink_path in pairs(self:get_sinks()) do
		local device = pulseaudio.device.create(self.connection, sink_path)
		self.outputs[sink_path] = device
	end

	for _, source_path in pairs(self:get_sources()) do
		local device = pulseaudio.device.create(self.connection, source_path)
		self.inputs[source_path] = device
	end

	return self
end

function pulseaudio.core:get_sinks()
	return self:Get("org.PulseAudio.Core1", "Sinks")
end

function pulseaudio.core:get_sources()
    return self:Get("org.PulseAudio.Core1", "Sources")
end

function pulseaudio.core:outputs_add(sink_path)
	local device = pulseaudio.device.create(self.connection, sink_path)
	self.outputs[sink_path] = output
end

function pulseaudio.core:outputs_remove(sink_path)
	self.outputs[sink_path] = nil
end

function pulseaudio.core:inputs_add(source_path)
	local device = pulseaudio.device.create(self.connection, source_path)
	if not device.Name or device.Name:match("%.monitor$") then
		return
	end

	self.inputs[source_path] = output
end

function pulseaudio.core:inputs_remove(source_path)
	if not self.inputs[source_path] then
		return
	end

	self.inputs[source_path] = nil
end

-- Interface functions
--------------------------------------------------------------------------------

-- Constructor
--------------------------------------------------------------------------------
function pulseaudio:init(on_outputs_changed, on_volume_changed, on_inputs_changed, on_mic_changed)
	local status, address = pcall(pulse.get_address)
	if not status then
		if iteration == 30 then
			naughty.notify({
				title = "Error while loading widget 'volume'",
				text = address,
				preset = naughty.config.presets.critical
			})
		else
			timer.single_shot(1, function()
				self:init_dbus(iteration + 1)
				return false
			end)
		end

		return
	end

	self.connection = pulse.get_connection(address)
	self.core = pulseaudio.core.create(self.connection)
end

return pulseaudio
