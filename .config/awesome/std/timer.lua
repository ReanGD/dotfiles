local gears = require("gears")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
timer = {}

-- Run callback after timeout and close timer
--------------------------------------------------------------------------------
function timer.single_shot(timeout, callback)
	gears.timer {
		timeout = timeout,
		autostart = true,
		single_shot = true,
		callback = callback
	}
end

return timer
