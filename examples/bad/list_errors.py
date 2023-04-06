a = []; # ERROR: cannot specify type
a = [1]; # OK
b = [int]; # OK
appendl(b, "str"); # ERROR: cannot append wrong type to list

c = [1, "czesc"]; # ERROR: list can only have one type

d = [1, 2, 3];
print(d[10]); # ERROR: out of bounds!