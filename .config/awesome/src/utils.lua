local io = io
local assert = assert

local utils = {}

function utils.file_write(path, data)
    local file = io.open(path, "w")

    if file then
        file:write(data)
        file:close()

        return true
    end
    
    return false
end

function utils.file_read(path)
    local file = io.open(path)

    if file then
        output = file:read("*a")
        file:close()
    else
        return nil
    end

    return output
end

function utils.output_read(cmd)
    local file = assert(io.popen(cmd, 'r'))
    local output = file:read('*all')
    file:close()

    return output
end

return utils
