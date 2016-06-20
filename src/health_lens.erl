-module(health_lens).

-export([
   focus/2
]).

focus({Mod, Fun}, Key)
 when is_atom(Mod), is_atom(Fun) ->
   Mod:Fun(clue:get(Key));

focus({Mod, Fun, X}, Key)
  when is_atom(Mod), is_atom(Fun) ->
   Mod:Fun(X, clue:get(Key));

focus(Fun, Key)
 when is_function(Fun) ->
   Fun(clue:get(Key));

focus({Fun, X}, Key)
 when is_function(Fun) ->
   Fun(X, clue:get(Key)).
