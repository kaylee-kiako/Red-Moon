print("registry debug:")
local registry = debug.getregistry()
registry["debug.lua value"] = 1
print("stored and retrieved: " .. registry["debug.lua value"])


print("\nmetatable debug:")
local test_table = {}
local metatable = { __metatable = 1 }

if debug.setmetatable(1, metatable) == 1 then
  print("debug.setmetatable(1) returns passed value")
end

if debug.getmetatable(1) == nil then
  print("debug.getmetatable(1) returns nil")
end

if debug.setmetatable(test_table, metatable) == test_table then
  print("debug.setmetatable(test_table, metatable)")
end

if debug.getmetatable(test_table) == metatable then
  print("debug.getmetatable(test_table) == metatable")
end

if getmetatable(test_table) == 1 then
  print("basic getmetatable() returns __metatable value")
end


print("\ninstruction count hook:")
debug.sethook(print, "", 1)
-- one instruction
local i = 0
-- four instructions
debug.sethook()


print("\nhooks paused in hook:")
local hook = function()
  print(1)
  debug.sethook(function() print("nothing") end, "", 1)
  print(2)
  debug.sethook()
end

debug.sethook(hook, "", 1)
i = i + 1
