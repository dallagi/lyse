-module(tree).
-export([empty/0, insert/3, lookup/2]).
 
empty() -> {node, 'nil'}.

insert(NewKey, NewVal, {node, 'nil'}) -> {node, {NewKey, NewVal, {node, 'nil'}, {node, 'nil'} }};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey > Key -> {node, {Key, Val, Smaller, insert(NewKey, NewVal, Larger)}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Larger}}) when NewKey < Key -> {node, {Key, Val, insert(NewKey, NewVal, Smaller), Larger}};
insert(Key, Val, {node, {Key, _, Smaller, Larger}}) -> {node, {Key, Val, Smaller, Larger}}.

% lookup(Key, Node)
lookup(_, {node, 'nil'}) -> undefined;
lookup(Key, {node, {Key, Val, _, _}}) -> {ok, Val};
lookup(Key, {node, {NodeKey, _, Smaller, _}}) when Key < NodeKey -> lookup(Key, Smaller);
lookup(Key, {node, {NodeKey, _, _, Larger}}) when Key > NodeKey -> lookup(Key, Larger).

