-module(functions).
-compile(export_all). %% TODO: replace later with -export() !!

head([H|_]) -> H.

second([_,X|_]) -> X.

