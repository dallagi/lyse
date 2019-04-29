-module(guards).
-export([old_enough/1, right_age/1]).

old_enough(X) when X >= 16 -> true;
old_enough(_) -> false.

right_age(Age) when Age >= 16, Age <= 104 -> true;
right_age(_) -> false.

wrong_age(Age) when Age < 16; X > 104 -> true;
wrong_age(Age) -> false.

