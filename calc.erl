-module(calc).
-export([rpn/1]).

rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.

read(X) ->
    case string:to_float(X) of
        {error, _} -> list_to_integer(X);
        {F, _} -> F
    end.

rpn("+", [N1, N2 | Rest]) -> [N1+N2 | Rest];
rpn("-", [N1, N2 | Rest]) -> [N1-N2 | Rest];
rpn("*", [N1, N2 | Rest]) -> [N1*N2 | Rest];
rpn("/", [N1, N2 | Rest]) -> [N1/N2 | Rest];
rpn("^", [N1, N2 | Rest]) -> [math:pow(N1, N2) | Rest];
rpn("ln", [N | Rest]) -> [math:log(N) | Rest];
rpn("log10", [N | Rest]) -> [math:log10(N) | Rest];
rpn("sum", Stack) -> [lists:sum(Stack)];
rpn("prod", Stack) -> [lists:foldl(fun(X, Prod) -> X * Prod end, 1, Stack)];
rpn(Num, Stack) -> [read(Num) | Stack].

