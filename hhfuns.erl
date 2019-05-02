-module(hhfuns).
-compile(export_all).
 
one() -> 1.
two() -> 2.
 
add(X,Y) -> X() + Y().

increment([]) -> [];
increment([Head | Tail]) -> [Head+1 | increment(Tail)].

decrement([]) -> [];
decrement([Head | Tail]) -> [Head-1 | decrement(Tail)].

map(_, []) -> [];
map(Function, [Head | Tail]) -> [Function(Head) | map(Function, Tail)].

incr(X) -> X + 1.
decr(X) -> X - 1.

filter(Pred, L) -> lists:reverse(filter(Pred, L,[])).
 
filter(_, [], Acc) -> Acc;
filter(Pred, [H|T], Acc) ->
  case Pred(H) of
    true  -> filter(Pred, T, [H|Acc]);
    false -> filter(Pred, T, Acc)
  end.
