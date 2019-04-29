-module(rec).
-compile(export_all).
%% -export([tail_fac/1, tail_len/1, tail_duplicate/2, reverse/1, tail_reverse/1, sublist/2, tail_sublist/2, zip/2]).

tail_fac(N) -> tail_fac(N, 1).
tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) when N > 0 -> tail_fac(N-1, N*Acc).

tail_len(List) -> tail_len(List, 0).
tail_len([], Acc) -> Acc;
tail_len([_ | Tail], Acc) -> tail_len(Tail, Acc+1).

tail_duplicate(N, Term) -> tail_duplicate(N, Term, []).
tail_duplicate(0, _, Acc) -> Acc;
tail_duplicate(N, Term, Acc) when N > 0 -> tail_duplicate(N-1, Term, [Term | Acc]).

reverse([]) -> [];
reverse([Head|Tail]) -> reverse(Tail) ++ [Head].

tail_reverse(List) -> tail_reverse(List, []).
tail_reverse([], Acc) -> Acc;
tail_reverse([Head|Tail], Acc) -> tail_reverse(Tail, [Head | Acc]).

sublist(_, 0) -> [];
sublist([], _) -> [];
sublist([ListHead | ListTail], N) when N > 0 -> [ ListHead | sublist(ListTail, N-1) ].

tail_sublist_1(List, N) -> tail_sublist_1(List, N, []).
tail_sublist_1(_, 0, Acc) -> Acc;
tail_sublist_1([], 0, Acc) -> Acc;
tail_sublist_1([ListH | ListT], N, Acc) -> tail_sublist_1(ListT, N-1, Acc ++ [ListH]).

tail_sublist(List, N) -> reverse(tail_sublist(List, N, [])).
tail_sublist(_, 0, Acc) -> Acc;
tail_sublist([], _, Acc) -> Acc;
tail_sublist([ListH | ListT], N, Acc) -> tail_sublist(ListT, N-1, [ListH | Acc]).

zip([], []) -> [];
zip([X | Xs], [Y | Ys]) -> [ {X, Y} | zip(Xs, Ys)].

lenient_zip([], _) -> [];
lenient_zip(_, []) -> [];
lenient_zip([X | Xs], [Y | Ys]) -> [ {X, Y} | lenient_zip(Xs, Ys)].

tail_zip(L1, L2) -> reverse(tail_zip(L1, L2, [])).
tail_zip([], [], Acc) -> Acc;
tail_zip([X | Xs], [Y | Ys], Acc) -> tail_zip(Xs, Ys, [ {X, Y} | Acc ]).

tail_lenient_zip(L1, L2) -> reverse(tail_lenient_zip(L1, L2, [])).
tail_lenient_zip([], _, Acc) -> Acc;
tail_lenient_zip(_, [], Acc) -> Acc;
tail_lenient_zip([X | Xs], [Y | Ys], Acc) -> tail_lenient_zip(Xs, Ys, [ {X, Y} | Acc ]).

quicksort([]) -> [];
quicksort([Pivot | Rest]) ->
    { Smaller, Larger } = partition(Rest, Pivot),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(List, Pivot) -> partition(List, Pivot, [], []).
partition([], _, Smaller, Larger) -> { Smaller, Larger };
partition([Element | Rest], Pivot, Smaller, Larger) ->
    if Element > Pivot -> partition(Rest, Pivot, Smaller, [Element|Larger]);
       Element =< Pivot -> partition(Rest, Pivot, [Element|Smaller], Larger)
    end.

