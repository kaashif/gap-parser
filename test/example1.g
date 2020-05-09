# this is a comment

# variable assignment
a := 1;

# float literal
b := 45.0;

# string literal
c := "hello my name is kaashif";

# some escaping
d := "here is a quote: \"quoted\"";

# function definition
f := function(x) return x+1; end;

# multiline function
g := function(p, q)
    return p;
end;

# function call
x := g(1, "one");

# no assignment function call
f(true);

# arithmetic expression
1 + 2;

# comparison
1 <= 2;

# boolean expression
true and false;

# comparisons and arithmetic
1+2 = 3 and true <> false and 4 > 3;

# if statement
if true then
    x := f(1);
fi;

# elif and else
if 1 = 2 then
    x := 1;
elif 3 = 4 then
    x := 3;
else
    x := 4;
fi;

# while
i := 0;
while i <= 100 do
    i := i + 1;
od;

# other operators
x mod 2;
1 in x;

# list literal
list := [1,2,3];

# heterogeneous list
list2 := [1, "aaa", false];
