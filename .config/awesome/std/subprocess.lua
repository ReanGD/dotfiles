-- Initialize tables and vars for module
--------------------------------------------------------------------------------
local io = io
local assert = assert

-- Constructor
--------------------------------------------------------------------------------
local subprocess = {}

-- Run command and read output
--------------------------------------------------------------------------------
function subprocess.output_read(cmd)
    local file = assert(io.popen(cmd, 'r'))
    local output = file:read('*all')
    file:close()

    return output
end

return subprocess
