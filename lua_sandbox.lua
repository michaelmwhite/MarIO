-- lua functions return nil if no explicit return statement???
function testReturn()
    x = 1
end

print(testReturn() == nil)