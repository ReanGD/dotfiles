local gears = require("gears")
local naughty = require("naughty")
local timer = require("std.timer")
local proxy = require("dbus_proxy")
local pulse = require("pulseaudio_dbus")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local pulseaudio = {}
pulseaudio.device = {}
pulseaudio.core = {}

local table = table

-- Local functions
--------------------------------------------------------------------------------
local function add_interface(instance, interface)
	for ind, attribute in pairs(interface) do
		assert(instance[ind] == nil, "Cannot override attribute " .. ind)
		instance[ind] = attribute
	end
end

-- pulseaudio.device functions
--------------------------------------------------------------------------------
function pulseaudio.device.create(connection, path)
	local instance = proxy.Proxy:new({
		bus = connection,
		name=nil,
		path=path,
		-- see: https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/Device/
		interface="org.PulseAudio.Core1.Device"
	})

	add_interface(instance, pulseaudio.device)

	return instance
end

function pulseaudio.device:get_volume()
	return self:Get("org.PulseAudio.Core1.Device", "Volume")[1]
end

-- pulseaudio.core functions
--------------------------------------------------------------------------------
function pulseaudio.core.create(connection)
	self = proxy.Proxy:new({
		bus = connection,
		name = nil, -- nil, because bus is *not* a message bus.
		path = "/org/pulseaudio/core1",
		-- see https://www.freedesktop.org/wiki/Software/PulseAudio/Documentation/Developer/Clients/DBus/Core/
		interface = "org.PulseAudio.Core1"
	})

	add_interface(self, pulseaudio.core)

	self.outputs = {}
	self.connection = connection

	self:ListenForSignal("org.PulseAudio.Core1.NewSink", {self.object_path})
	self:ListenForSignal("org.PulseAudio.Core1.SinkRemoved", {self.object_path})

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

	for _, sink_path in pairs(self:get_sinks()) do
		local device = pulseaudio.device.create(self.connection, sink_path)
		self.outputs[sink_path] = device
		gears.debug.print_error("core.create: device = " .. device.Name .. ", path = " .. sink_path)
	end

	return self
end

function pulseaudio.core:get_sinks()
	return self:Get("org.PulseAudio.Core1", "Sinks")
end

function pulseaudio.core:outputs_add(sink_path)
	local device = pulseaudio.device.create(self.connection, sink_path)
	self.outputs[sink_path] = output
	gears.debug.print_error("outputs_add: device = " .. device.Name .. ", path = " .. sink_path)
end

function pulseaudio.core:outputs_remove(sink_path)
	self.outputs[sink_path] = nil
	gears.debug.print_error("outputs_remove: path = " .. sink_path)
end

-- Interface functions
--------------------------------------------------------------------------------

-- Constructor
--------------------------------------------------------------------------------
function pulseaudio:init(update_func)
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
