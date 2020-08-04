local awful = require("awful")
local naughty = require("naughty")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local subprocess = {}

local io = io
local assert = assert
local string = string

-- Run command and read output
--------------------------------------------------------------------------------
function subprocess.output_read(cmd)
	local file = assert(io.popen(cmd, "r"))
	local output = file:read("*all")
	file:close()

	return output
end

-- Run command if it not running
--------------------------------------------------------------------------------
function subprocess.run_once(cmd, args)
	args = args or {}

	if not cmd or cmd == "" then
		naughty.notify({
			title = "Error while run autostart app",
			text = "arg 'cmd' for run_once empty or nil",
			preset = naughty.config.presets.critical
		})
		return
	end

	local process_name = args.process_name or cmd
	local run_cmd = string.format("pgrep -u $USER %s > /dev/null || %s", process_name, cmd)

	local params = {}
	if args.tag then
		params.tag = args.tag
	end

	awful.spawn.spawn({ awful.util.shell, "-c", run_cmd }, params)
end

return subprocess
