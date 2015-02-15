%%
%%   Copyright (c) 2012 - 2015, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @description
%%   health library benchmark 
-module(health_benchmark).

-export([run/0]).
-compile(inline).

-define(N,        8).
-define(LOOP,     3 * 1000 * 1000).
-define(TIMEOUT, 60 * 1000).

%%
%%
run() ->
   health:start(),
   {ok, _} = health:start_link({test, 1, {check, 500}}),
   clue:put(test, 0),
   case timer:tc(fun() -> run(?N) end) of
      {T, ok} ->
         TPU = ?N * ?LOOP / T,
         TPS = ?N * ?LOOP / (T / 1000000),
         {TPU, TPS};
      {_, Error} ->
         Error
   end.

run(N) ->
   Self = self(),
   Pids = [spawn_link(fun() -> loop(Self, ?LOOP) end) || _ <- lists:seq(1, N)],
   fold(Pids).

fold([]) -> ok;
fold([Pid | Pids]) ->
   receive
      {ok, Pid} -> fold(Pids)
   after ?TIMEOUT ->
      {error, timeout}
   end.

loop(Pid,  0) ->
   Pid ! {ok, self()};
loop(Pid,  N) ->
   _ = health:check(test),
   loop(Pid, N - 1).


