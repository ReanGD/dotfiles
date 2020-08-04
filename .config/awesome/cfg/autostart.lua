local subprocess = require("std.subprocess")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local autostart = {}

-- Constructor
--------------------------------------------------------------------------------
function autostart:init(args)
	args = args or {}

	subprocess.run_once("firefox", { tag = "web" })
	subprocess.run_once("subl3", { tag = "doc" })
	subprocess.run_once("doublecmd", { tag = "cmdr" })
	subprocess.run_once("perWindowLayoutD", { process_name = "perWindowLayout" })
end

return autostart
