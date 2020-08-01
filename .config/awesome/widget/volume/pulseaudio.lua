local lgi =  require("lgi")
local naughty = require("naughty")
local timer = require("std.timer")
local proxy = require("dbus_proxy")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local pulseaudio = {}
pulseaudio.port = {}
pulseaudio.device = {}
pulseaudio.core = {}

local math = math
local table = table
local string = string

-- Local functions
--------------------------------------------------------------------------------
local function add_interface(instance, interface)
	for ind, attribute in pairs(interface) do
		assert(instance[ind] == nil, "Cannot override attribute " .. ind)
		instance[ind] = attribute
	end
end

local function get_address()
	local server = proxy.Proxy:new({
		bus=proxy.Bus.SESSION,
		name="org.PulseAudio1",
		path="/org/pulseaudio/server_lookup1",
		interface="org.PulseAudio.ServerLookup1"
	})

	return server.Address
end

local function get_volume_percent(volume, base_volume)
	return math.max(0, math.ceil(volume * 100.0 / base_volume - 0.5))
end

local function on_item_changed(device)
end

local function on_array_changed(inputs, outputs)
end

-- pulseaudio.port
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

-- pulseaudio.device
--------------------------------------------------------------------------------
function pulseaudio.device.create(connection, path, settings, is_output)
	local self = proxy.Proxy:new({
		bus = connection,
		name=nil,
		path=path,
		-- see: https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/Device/
		interface="org.PulseAudio.Core1.Device"
	})

	add_interface(self, pulseaudio.device)

	self.settings = settings
	self.is_output = is_output
	self.connection = connection
	self.mute = self:_get_mute()
	self.volume_percent = 0
	self.active_port_desc = "<unknown>"

	self:_update_volume_percent()
	self:_update_active_port_desc()

	if self.signals.MuteUpdated then
		self:connect_signal(
			function (this, is_mute)
				if this.object_path == self.object_path then
					self.muted = is_mute
					self:_update_volume_percent()
					self.settings.on_device_changed(self)
				end
			end,
			"MuteUpdated"
		)
	end

	if self.signals.VolumeUpdated then
		self:connect_signal(
			function (this, volumes)
				if this.object_path == self.object_path then
					self:_update_volume_percent(tonumber(volumes[1]))
					self.settings.on_device_changed(self)
				end
			end,
			"VolumeUpdated"
		)
	end

	if self.signals.ActivePortUpdated then
		self:connect_signal(
			function (this, port_path)
				if this.object_path == self.object_path then
					self:_update_active_port_desc(port_path)
					self.settings.on_device_changed(self)
				end
			end,
			"ActivePortUpdated"
		)
	end

	return self
end

function pulseaudio.device:_update_active_port_desc(port_path)
	if #self.Ports == 0 then
		self.active_port_desc = "<unknown>"
	else
		local path = port_path or self:Get("org.PulseAudio.Core1.Device", "ActivePort")
		self.active_port_desc = pulseaudio.port.create(self.connection, path).Description
	end
end

function pulseaudio.device:_get_volumes()
	return self:Get("org.PulseAudio.Core1.Device", "Volume")
end

function pulseaudio.device:_set_volumes(value)
	self:Set("org.PulseAudio.Core1.Device", "Volume", lgi.GLib.Variant("au", value))
	self.Volume = {signature="au", value=value}
end

function pulseaudio.device:_update_volume_percent(volume)
	local value = volume or self:_get_volumes()[1]
	local base_volume = self.BaseVolume
	if not self.is_output then
		base_volume = base_volume * 10
	end
	self.volume_percent = get_volume_percent(value, base_volume)
end

function pulseaudio.device:set_volume_percent(volume_percent)
	local new_volumes = {}
	local cur_volumes = self:_get_volumes()
	local base_volume = self.BaseVolume
	if not self.is_output then
		base_volume = base_volume * 10
	end
	local raw_volume = volume_percent * base_volume / 100
	for key, _ in ipairs(cur_volumes) do
		new_volumes[key] = raw_volume
	end
	self:_set_volumes(new_volumes)
	self.volume_percent = volume_percent
end

function pulseaudio.device:volume_up()
	if self.mute then
		return
	end

	local volume_percent = self.volume_percent + self.settings.volume_step
	if volume_percent > self.settings.volume_max then
		volume_percent = self.settings.volume_max
	elseif volume_percent > 100 and self.volume_percent < 100 and self.settings.volume_max > 100 then
		volume_percent = 100
	end

	self:set_volume_percent(volume_percent)
end

function pulseaudio.device:volume_down()
	if self.mute then
		return
	end

	local volume_percent = self.volume_percent - self.settings.volume_step
	if volume_percent < 100 and self.volume_percent > 100 then
		volume_percent = 100
	elseif volume_percent < 0 then
		volume_percent = 0
	end

	self:set_volume_percent(volume_percent)
end

function pulseaudio.device:_get_mute()
	return self:Get("org.PulseAudio.Core1.Device", "Mute")
end

function pulseaudio.device:set_mute(value)
	self:Set("org.PulseAudio.Core1.Device", "Mute", lgi.GLib.Variant("b", value))
	self.Mute = {signature="b", value=value}
	self.mute = value
end

function pulseaudio.device:toggle_mute()
	local mute = self:_get_mute()
	self:set_mute(not mute)
end

-- pulseaudio.core
--------------------------------------------------------------------------------
function pulseaudio.core.create(connection, settings)
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
	self.settings = settings

	self:ListenForSignal("org.PulseAudio.Core1.Device.MuteUpdated", {})
	self:ListenForSignal("org.PulseAudio.Core1.Device.VolumeUpdated", {})
	self:ListenForSignal("org.PulseAudio.Core1.Device.ActivePortUpdated", {})
	self:ListenForSignal("org.PulseAudio.Core1.NewSink", { self.object_path })
	self:ListenForSignal("org.PulseAudio.Core1.SinkRemoved", { self.object_path })
	self:ListenForSignal("org.PulseAudio.Core1.NewSource", { self.object_path })
	self:ListenForSignal("org.PulseAudio.Core1.SourceRemoved", { self.object_path })

	self:connect_signal(
		function (_, sink_path)
			self:_outputs_add(sink_path)
		end,
		"NewSink"
	)

	self:connect_signal(
		function (_, sink_path)
			self:_outputs_remove(sink_path)
		end,
		"SinkRemoved"
	)

	self:connect_signal(
		function (_, source_path)
			self:_inputs_add(source_path)
		end,
		"NewSource"
	)

	self:connect_signal(
		function (_, source_path)
			self:_inputs_remove(source_path)
		end,
		"SourceRemoved"
	)

	is_output = true
	for _, sink_path in pairs(self:_get_sinks()) do
		local device = pulseaudio.device.create(self.connection, sink_path, self.settings, is_output)
		table.insert(self.outputs, device)
	end

	is_output = false
	for _, source_path in pairs(self:_get_sources()) do
		local device = pulseaudio.device.create(self.connection, source_path, self.settings, is_output)
		if device.Name and not device.Name:match("%.monitor$") then
			table.insert(self.inputs, device)
		end
	end

	self.settings.on_devices_changed(self.inputs, self.outputs)

	return self
end

function pulseaudio.core:_get_sinks()
	return self:Get("org.PulseAudio.Core1", "Sinks")
end

function pulseaudio.core:_get_sources()
    return self:Get("org.PulseAudio.Core1", "Sources")
end

function pulseaudio.core:_outputs_add(sink_path)
	is_output = true
	local device = pulseaudio.device.create(self.connection, sink_path, self.settings, is_output)
	self.outputs[device.Index] = device
	table.insert(self.outputs, device)
	self.settings.on_devices_changed(self.inputs, self.outputs)
end

function pulseaudio.core:_outputs_remove(sink_path)
	for ind, device in pairs(self.outputs) do
		if device.object_path == sink_path then
			table.remove(self.outputs, ind)
			self.settings.on_devices_changed(self.inputs, self.outputs)
		end
	end
end

function pulseaudio.core:_inputs_add(source_path)
	is_output = false
	local device = pulseaudio.device.create(self.connection, source_path, self.settings, is_output)
	if not device.Name or device.Name:match("%.monitor$") then
		return
	end

	table.insert(self.inputs, device)
	self.settings.on_devices_changed(self.inputs, self.outputs)
end

function pulseaudio.core:_inputs_remove(source_path)
	for ind, device in pairs(self.inputs) do
		if device.object_path == source_path then
			table.remove(self.inputs, ind)
			self.settings.on_devices_changed(self.inputs, self.outputs)
		end
	end
end

function pulseaudio.core:volume_up()
	for _, device in pairs(self.outputs) do
		device:volume_up()
	end
end

function pulseaudio.core:volume_down()
	for _, device in pairs(self.outputs) do
		device:volume_down()
	end
end

function pulseaudio.core:toggle_mute()
	for _, device in pairs(self.outputs) do
		device:toggle_mute()
	end
end

function pulseaudio.core:volume_up_mic()
	for _, device in pairs(self.inputs) do
		device:volume_up()
	end
end

function pulseaudio.core:volume_down_mic()
	for _, device in pairs(self.inputs) do
		device:volume_down()
	end
end

function pulseaudio.core:toggle_mute_mic()
	for _, device in pairs(self.inputs) do
		device:toggle_mute()
	end
end

-- Interface functions
--------------------------------------------------------------------------------
function pulseaudio:volume_up()
	self.core:volume_up()
end

function pulseaudio:volume_down()
	self.core:volume_down()
end

function pulseaudio:toggle_mute()
	self.core:toggle_mute()
end

function pulseaudio:volume_up_mic()
	self.core:volume_up_mic()
end

function pulseaudio:volume_down_mic()
	self.core:volume_down_mic()
end

function pulseaudio:toggle_mute_mic()
	self.core:toggle_mute_mic()
end

-- Module functions
--------------------------------------------------------------------------------
function pulseaudio:_init_iteration(settings, iteration)
	local status, address = pcall(get_address)
	if not status then
		if iteration == 30 then
			naughty.notify({
				title = "Error while loading widget 'volume'",
				text = address,
				preset = naughty.config.presets.critical
			})
		else
			timer.single_shot(1, function()
				self:_init_iteration(settings, iteration + 1)
				return false
			end)
		end

		return
	end

	local connection = lgi.Gio.DBusConnection.new_for_address_sync(address, lgi.Gio.DBusConnectionFlags.AUTHENTICATION_CLIENT)
	assert(not connection.closed, string.format("Connection from '%s' is closed!", address))

	self.core = pulseaudio.core.create(connection, settings)
end

-- Constructor
--------------------------------------------------------------------------------
function pulseaudio:init(on_device_changed, on_devices_changed, volume_step, volume_max)
	local settings = {}
	settings.on_device_changed = on_device_changed or on_item_changed
	settings.on_devices_changed = on_devices_changed or on_array_changed
	settings.volume_step = volume_step or 5
	settings.volume_max = volume_max or 150

	self:_init_iteration(settings, 0)
end

return pulseaudio
