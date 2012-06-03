local lfs = require "lfs"

local trigraphs = {
	["="] = "#" ;
	["/"] = "\\" ;
	["'"] = "^" ;
	["("] = "[" ;
	[")"] = "]" ;
	["!"] = "|" ;
	["<"] = "{" ;
	[">"] = "}" ;
	["-"] = "~" ;
}

local include_search_paths = {
	"/usr/local/include/" ;
	"/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.0/include/" ;
	"/usr/include/linux/" ;
	"/usr/include/" ;
}

local cache = { }

function preprocess ( path , defines )
	local r = cache [ path ]
	if r then return r end
	r = {
		defines = defines or { } ;
		macros = { } ;
	}
	cache [ path ] = r

	local fd = assert ( io.open ( path ) )
	local file = assert ( fd:read ( "*a" ) )

	-- Trigraph replacement
	file = file:gsub("%?%?[?/'()!<>-]",trigraphs)

	-- Line splicing
	file = file:gsub("\\\n","")

	-- Comment removal
	file = file:gsub("//.-\n","\n")
	file = file:gsub("/%*.-%*/","")

	-- State variables
	local nest_level = 0
	local ignore = false

	local function prep ( str )
		local init = 1
		local ret = { }
		while true do
			local s , e , token = str:find ( "([%w_]+)" , init )
			if not s then break end
			table.insert ( ret , str:sub ( init , s-1 ) )

			local val = r.defines[token]
			if val then
				table.insert ( ret , val )
			else
				local macro = r.macros[token]
				--print('"'..token..'"',macro)
				if macro then
					-- TODO: Improve
					as , ae = str:find ( "%b()" , e+1 )
					if as then
						local args = { }
						for arg in str:sub(as+1,ae-1):gmatch("[^,]+") do
							table.insert ( args , arg )
						end
						table.insert ( ret , macro ( unpack ( args ) ) )
						e = ae
					else
						table.insert ( ret , token )
					end
				else -- Nothing special about this token
					table.insert ( ret , token )
				end
			end
			init = e + 1
		end
		table.insert ( ret , str:sub ( init , -1 ) )
		return table.concat ( ret )
	end

	for line in string.gmatch ( file , "[^\n]*" ) do
		local command , data = line:match ( "^#%s*(%S+)%s*(.-)%s*$" )
		if command then
			if command == "ifdef" or command == "ifndef" or command == "if" then
				nest_level = nest_level + 1
			end
			if ignore then
				if nest_level == ignore then
					if command == "endif" or command == "else" then
						ignore = false
					elseif command == "elif" then
					end
				end
			elseif command == "define" then
				local name , val
				-- Macros
				local args
				name , args , val = data:match("^([%w_]+)%((.-)%)%s*(.-)%s*$")
				if name then
					print("NEW MACRO",name,line)
					local val = function ( ... )
						--print(name,"IN",...)
						local replace_args = { ... }
						local i = 1
						local ret = val
						for arg in args:gmatch("[^,]+") do
							local new_val = replace_args [ i ]
							ret = ret:gsub ( "%f[%w_]" .. arg .. "%f[^%w_]" , new_val )
							i = i + 1
						end
						--print(name,"OUT",ret,line)
						return ret
					end
					r.macros [ name ] = val
				else
					-- Standard defines
					local name , val = data:match("^([%w_]+)%s*(.-)%s*$")
					val = prep ( val )
					if val:match("^0%d+$") then
						val = tonumber(val,8)
					else
						val = tonumber(val) or val
					end
					r.defines [ name ] = val
				end
			elseif command == "ifdef" then
				if not r.defines [ data ] then
					ignore = nest_level
				end
			elseif command == "ifndef" then
				if r.defines [ data ] then
					ignore = nest_level
				end
			elseif command == "else" then
				ignore = nest_level
			elseif command == "if" or command == "elif" then
				data = prep ( data )
				print("IF",data)
			elseif command == "include" then
				local include_path = data:match('"(.*)"')
				if not include_path then
					include_path = data:match('<(.*)>')
					assert ( include_path , '#include expects "FILENAME" or <FILENAME>' )
					local err
					for i , include_dir in ipairs ( include_search_paths ) do
						local try_path = include_dir .. include_path
						local mode
						mode , err = lfs.attributes ( try_path , "mode" )
						if mode then
							include_path = try_path
							break
						end
					end
					assert ( include_path , err )
				end
				preprocess ( include_path , defines )
			end
			if command == "endif" then
				nest_level = nest_level - 1
			end
		end
	end
	return r
end

local r = preprocess(assert(arg[1]))
for k , v in pairs(r.defines) do print(k,v) end
