local utils = require("src.utils")

local tonumber = tonumber
local os = os

local timestamp = {}

local tmp_path = "/tmp/awesome-stamp"
local timeout = 5

function timestamp.make()
    utils.file_write(tmp_path, os.time())
end

function timestamp.is_startup()
    res = utils.file_read(tmp_path)
    if res then
        return (os.time() - tonumber(res)) > timeout
    end

    return true
end

awesome.connect_signal("exit",
    function()
        timestamp.make()
    end
)

return timestamp
