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

local system_include_search_paths = {
	"/usr/local/include/" ;
	"/usr/lib/gcc/x86_64-unknown-linux-gnu/4.7.1/include/" ;
	"/usr/include/" ;
	--"/usr/include/linux/" ;
}
local include_search_paths = {
	"./" ;
}

local function findfile ( name , paths )
	local err
	for i , include_dir in ipairs ( paths ) do
		local try_path = include_dir .. name
		local mode
		mode , err = lfs.attributes ( try_path , "mode" )
		if mode then
			return try_path
		end
	end
	return nil , err
end

local lpeg = require "lpeg"
local C = lpeg.C
local Carg = lpeg.Carg
local P = lpeg.P
local R = lpeg.R
local S = lpeg.S
local V = lpeg.V
local xdigit = R ( "09" , "af" , "AF" )

local function make_error ( msg )
	return function ( s , i , ... )
			error ( "error at pos " .. i .. ": " .. msg , 2 )
		end
end

local comment_removal = lpeg.Cs {
	block_comment = P"/*" * ( 1 - P"*/" )^0 * P"*/" ;
	line_comment = P"//" * ( 1 - P"\n" )^0 ;
	( ( V"block_comment" + V"line_comment" ) / "" + 1 ) ^0 ;
}

--[[
"expression is a C expression of integer type, subject to stringent restrictions.
It may contain
 - Integer constants, which are all regarded as long or unsigned long.
 - Character constants, which are interpreted according to the character set
   and conventions of the machine and operating system on which the
   preprocessor is running.
   The GNU C preprocessor uses the C data type `char' for these character
   constants; therefore, whether some character codes are negative is
   determined by the C compiler used to compile the preprocessor. If it treats
   `char' as signed, then character codes large enough to set the sign bit will
   be considered negative; otherwise, no character code is considered negative.
 - Arithmetic operators for addition, subtraction, multiplication, division,
   bitwise operations, shifts, comparisons, and logical operations (`&&' and `||').
 - Identifiers that are not macros, which are all treated as zero(!).
 - Macro calls. All macro calls in the expression are expanded before actual
   computation of the expression's value begins.

Note that `sizeof' operators and enum-type values are not allowed.

enum-type values, like all other identifiers that are not taken as macro calls
and expanded, are treated as zero.
]]

local Space = S(" \t")^0
local Number = (
		C(P"-"^-1 * R("09")^1) / tonumber +
		C(P"-"^-1 * P"0x" * xdigit^1) / function ( n ) return tonumber ( n , 16 ) end +
		C(P"-"^-1 * P"0" * R("09")^1) / function ( n ) return tonumber ( n , 8 ) end
	) * Space
local Identifier = C ( ( R("az","AZ") + S"_" ) * ( R("az","AZ","09") + S"_" )^0 )
local escape_tbl = {
	a = "\a" ;
	b = "\b" ;
	f = "\f" ;
	n = "\n" ;
	r = "\r" ;
	t = "\t" ;
	v = "\v" ;
	["'"] = "'" ;
	['"'] = '"' ;
	["\\"] = "\\" ;
	["?"] = "?" ;
}
local Character = P"'" * ( ( P"\\"*(
		R("09")*R("09")*R("09") / function(n) return string.char(tonumber(n,8)) end + --Octal
		P"x"*xdigit*xdigit / function(n) return string.char(tonumber(n,16)) end + -- Hex
		P(1) / escape_tbl
	) + C(1) ) - "'" ) * P"'" / string.byte ;
local String_literal = P'"' * lpeg.Cs(((lpeg.P(1) - '"') + lpeg.P'\\"' / '"')^0) * P'"' ;

local bit = require "bit"
local lshift = bit.lshift
local rshift = bit.rshift
local band = bit.band
local bor = bit.bor
local bnot = bit.bnot
local bxor = bit.bxor

-- The unary operators (all have same precendence)
local unops = S"-!~"

local function unop ( op , v1 )
	if op == "-" then return -v1
	elseif op == "!" then return v1 == 0 and 1 or 0
	elseif op == "~" then return bnot ( v1 )
	end
end

-- Table of binary operators, ordered from lowest precedence to highest
local binops = {
	P"||" ;
	P"&&" ;
	P"|"-#P"|" ;
	P"^" ;
	P"&"-#P"&" ;
	P"==" + P"!=" ;
	P"<=" + P">=" + S"<>" ;
	P"<<" + P">>" ;
	S"+-" ;
	S"*/%" ;
}

local function binop (v1, op, v2)
	if op == "*" then return v1 * v2
	elseif op == "/" then return math.floor(v1 / v2)
	elseif op == "%" then return v1 % v2
	elseif op == "+" then return v1 + v2
	elseif op == "-" then return v1 - v2
	elseif op == "<<" then return lshift(v1,v2)
	elseif op == ">>" then return rshift(v1,v2)
	elseif op == "<=" then return v1 <= v2 and 1 or 0
	elseif op == ">=" then return v1 >= v2 and 1 or 0
	elseif op == "<" then return v1 < v2 and 1 or 0
	elseif op == ">" then return v1 > v2 and 1 or 0
	elseif op == "==" then return v1 == v2 and 1 or 0
	elseif op == "!=" then return v1 ~= v2 and 1 or 0
	elseif op == "&" then return band ( v1 , v2 )
	elseif op == "^" then return bxor ( v1 , v2 )
	elseif op == "|" then return bor ( v1 , v2 )
	elseif op == "&&" then return ( v1 ~= 0 and v2 ~= 0 ) and 1 or 0
	elseif op == "||" then return ( v1 ~= 0 or v2 ~= 0 ) and 1 or 0
	end
end

local G
G = {
	defined = P"defined"*Carg(1)*Space*(
			P"("*Space*Identifier*Space*P")" +
			Identifier +
			make_error[[operator "defined" requires an identifier]]
		)*Space / function ( state , name )
			if state.defines[name] == nil then
				return 0
			else
				return 1
			end
		end ;

	macro = Carg(1)*Identifier*Space*P"("*Space*V(1)* (P","*Space*V(1))^0 *P")"*Space /
	 	function ( state , id , ... )
	 		local func = state.macros[id]
	 		if func then
	 			local res = func ( ... )
	 			return G:match(res,1,state) -- Need to recursivly parse output of macro
	 		else
	 			error("unknown macro: " .. id)
	 		end
	 	end ;

	identifier = Carg(1)*Identifier*Space / function(state,id)
			local d = state.defines[id]
			if d == nil then
				return 0 -- If an identifer has not been defined it is 0
			else
				return G:match(d,1,state)
			end
		end ;
}
for i , patt in ipairs ( binops ) do
	G [ i ] = lpeg.Cf ( V(i+1) * lpeg.Cg( C(patt)*Space * V(i+1))^0 , binop )
end
local index = #G + 1
G [ index ] = lpeg.Cf ( (C(unops)*Space)^0 * V(index+1) , unop )
G [ index + 1 ] = V"defined" +
	V"macro" +
	V"identifier" +
	Number +
	Character +
	P"(" * Space * V(1) * P")" * Space

G = P ( G )

-- A grammar that does replacements in C code (has to exclude matches inside of strings etc)
local repl
repl = lpeg.Cs {
	V"token"^0 ;

	token = ( V"macro" + V"defined" ) +
		Identifier*Space + String_literal*Space + Character*Space + Number*Space + P(1) ;

	macro = lpeg.Cmt ( Carg(1)*Identifier*Space*P"("*Space*V"token"* (P","*Space*V"token")^0 *P")"*Space ,
			function ( s , i , state , id , ... )
				local m = state.macros[id]
				return m ~= nil , m , ...
			end ) / function ( m , ... )
				return m(...)
			end ;

	defined = lpeg.Cmt ( Carg(1)*Identifier ,
			function ( s , i , state , id )
				local d = state.defines[id]
				if d == nil then
					return false
				else
					--d = repl:match ( d , 1 , state )
					return true , d
				end
			end )*Space;
}

local function parser ( state , str )
	return lpeg.match ( G , str , 1 , state )
end

local function preprocess ( path , r )
	r = r or { }
	r.defines = r.defines or { } ;
	r.defines.__FILE__ = path
	r.macros  = r.macros or { } ;

	local fd = assert ( io.open ( path ) )
	local file = assert ( fd:read ( "*a" ) )

	-- Trigraph replacement
	file = file:gsub("%?%?[?/'()!<>-]",trigraphs)

	-- Line splicing
	file = file:gsub("\\\n","")

	assert ( file:sub(-1) == "\n" , "File should end in newline" )

	-- Comment removal
	file = comment_removal:match(file)

	-- State variables
	local nest_level = 0
	local ignore = false

	for line in string.gmatch ( file , "(.-)\n" ) do
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
						if parser ( r , data ) ~= 0 then
							ignore = false
						end
					end
				end
			elseif command == "error" then
				error ( data )
			elseif command == "define" then
				local name , val
				-- Macros
				local args
				name , args , val = data:match("^([%w_]+)%((.-)%)%s*(.-)%s*$")
				if name then
					assert ( name ~= "defined" , [["defined" cannot be used as a macro name]] )
					local arglist = { }
					for arg in args:gmatch("[^,]+") do
						arglist [ #arglist + 1 ] = arg
					end

					local val = function ( ... )
						local replace_args = { ... }
						if #arglist ~= #replace_args then
							error ( "macro requires " .. #arglist .. " arguments, but " .. #replace_args .. " given" )
						end
						local arg_tbl = setmetatable ( { } , {
								__index = function ( t , k ) return r.defines [ k ] or k end ;
							} )
						for i , arg in ipairs ( arglist ) do
							arg_tbl [ arg ] = replace_args [ i ]
						end
						return repl:match ( val , 1 , {defines=arg_tbl , macros=r.macros} )
					end
					r.macros [ name ] = val
				else
					-- Standard defines
					name , val = data:match("^([%w_]+)%s*(.-)%s*$")
					--val = parser ( r , val )
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
				if parser ( r , data ) == 0 then
					ignore = nest_level
				end
			elseif command == "include" then
				local include_path = data:match('"(.*)"')
				if include_path then
					include_path = assert ( findfile ( include_path , include_search_paths ) )
				else
					include_path = data:match('<(.*)>')
					assert ( include_path , '#include expects "FILENAME" or <FILENAME>' )
					include_path = assert ( findfile ( include_path , system_include_search_paths ) )
				end
				preprocess ( include_path , r )
				r.defines.__FILE__ = path
			end
			if command == "endif" then
				nest_level = nest_level - 1
			end
		elseif not ignore and #line > 0 then
			line = repl:match(line,1,r)
			print(line)
		end
	end
	return r
end

table.insert ( include_search_paths , "/usr/include/" )
local r = preprocess(assert(arg[1]),{
		defines = {
			__STDC__ = true ;
		}
	})
--for k , v in pairs(r.defines) do print(k,v) end
