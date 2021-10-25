local M = {}

local function scandir(directory)
    local i, t, popen = 0, {}, io.popen
    local pfile = popen('ls -LR "'..directory..'"')
		local p = 'nil'
    for f in pfile:lines() do
			if string.match(f, ".lua") and not(string.match(f, "lua:")) then
				if string.match(f, "/") then
					local j = 0
					local tmp = {}
					for str in string.gmatch(f, "([^/]+)") do
						j = j + 1
						table.insert(tmp, str)
					end
					p = tmp[j]:gsub("%:", "")
				else
					if not string.match(f, "au.lua") then
						t[f:gsub("%.lua","")] = p
					end
				end
			end
    end
    pfile:close()
    return t
end

M.reload = function()
	local t = scandir(vim.fn.stdpath('config') .. '/lua')
	for f,p in pairs(t) do
		local s
		if p == 'nil' then
			s = f
		else
			s = p .. '.' .. f
		end
		vim.cmd(':lua package.loaded["' .. s .. '"] = nil')
	end
	vim.cmd(':source ~/.config/nvim/init.lua')
	vim.cmd(':echo "Reloaded nvim config."')
end

return M
