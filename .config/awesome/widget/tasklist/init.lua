local awful = require("awful")

-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local tasklist = {}

local mouse = mouse
local client = client
local screen = screen

-- Local functions
--------------------------------------------------------------------------------
local function get_screen(s)
	return s and screen[s]
end

function focused_screen()
	return get_screen(client.focus and client.focus.screen or mouse.screen)
end

-- Module functions
--------------------------------------------------------------------------------
function tasklist:widget(s)
	return awful.widget.tasklist {
		screen = s,
		filter = function(c, s) return self:filter(c, s) end,
	}
end

function tasklist:filter(c, s)
	if get_screen(c.screen) ~= s then
		return false
	end

	if focused_screen() == s then
		if client.focus == c then
			s.last_focused_client = c
			return true
		end
	else
		return c == s.last_focused_client
	end

	return false
end

-- Constructor
--------------------------------------------------------------------------------
function tasklist:init(args)
	args = args or {}
end

return tasklist
