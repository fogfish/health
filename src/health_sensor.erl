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
%%   sensor process - polls system status variable 
-module(health_sensor).
-behaviour(pipe).

-export([
   start_link/1,
   init/1,
   free/2,
   ioctl/2,
   active/3,
   broken/3
]).

-record(fsm, {
   mod     = undefined :: any(),      %% functor to use
   key     = undefined :: any(),      %% key to poll
   safety  = undefined :: float(),    %% expected safety range
   freq    = undefined :: integer(),  %% frequency to poll value
   r       = undefined :: integer(),  %% max failures (r) occurs within (t) seconds
   t       = undefined :: integer(),  %% 
   failure = []        :: list(),     %% log of recent failures
   timer   = undefined :: _
}).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Spec) ->
   pipe:start_link(?MODULE, [Spec], []).

init([{Key, Safety, Strategy}]) ->
   pns:register(health, Key, self()),
   erlang:process_flag(priority, low),
   erlang:send(self(), check),
   {ok, active, 
      strategy(Strategy,
         #fsm{
            mod    = handler(Strategy, Key),
            key    = Key,
            safety = Safety
         }
      )
   }.


free(_, _) ->
   ok.

ioctl(_, _) ->
   throw(not_implemented).

%%%----------------------------------------------------------------------------   
%%%
%%% fsm
%%%
%%%----------------------------------------------------------------------------   

%%
%%
active(check, _, #fsm{mod = Mod, key = Key}=State0) ->
   case is_key_valid(State0) of
      undefined ->
         {next_state,  active, timeout(State0)};

      true  ->
         update(State0, ok),
         {next_state,  active, timeout(State0)};

      false ->
         update(State0, failed),
         case 
            is_failed(State0)
         of
            {true,  State1} ->
               {stop, {unhealth, Key, Mod:get(Key)}, State1};
            {false, State1} ->
               {next_state, broken, timeout(State1)}
         end
   end;

active(break, Pipe, #fsm{mod = Mod, key = Key}=State0) ->
   update(State0, failed),
   pipe:ack(Pipe, ok),
   case 
      is_failed(State0)
   of
      {true,  State1} ->
         {stop, {unhealth, Key, Mod:get(Key)}, State1};
      {false, State1} ->
         {next_state, broken, timeout(State1)}
   end;
   
active(_, _, State) ->
   {next_state, broken, State}.


%%
%%
broken(check, _, State) ->
   case is_key_valid(State) of
      undefined ->
         {next_state,  broken, timeout(State)};

      true  ->
         update(State, ok),
         {next_state,  broken, timeout(State)};

      false ->
         {next_state,  broken, timeout(State)}
   end;

broken(break, Pipe, State) ->
   pipe:ack(Pipe, ok),
   {next_state, broken, timeout(State)};

broken(_, _, State) ->
   {next_state, broken, State}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% re-write custom module
handler({lens, _, Fun}, _Key) ->
   {health_lens, focus, Fun};

handler(_, Key)
 when is_tuple(Key) ->
   case erlang:element(1, Key) of
      sys ->
         {health_sys, erlang:element(2, Key)};

      app ->
         {erlang:element(2, Key), erlang:element(3, Key)};

      _   ->
         {clue, get}
   end;

handler(_, _) ->
   {clue, get}.

%%
%% define sensor strategy
strategy({check, T}, State) ->
   State#fsm{freq = T};

strategy({supervise, T, MaxR, MaxT}, State) ->
   State#fsm{freq = T, r = MaxR, t = MaxT};

strategy({lens, T, _}, State) ->
   State#fsm{freq = T}.


%%
%%
timeout(#fsm{freq = Freq, timer = undefined} = State) ->
   State#fsm{
      timer = erlang:send_after(Freq, self(), check)
   };
timeout(#fsm{freq = Freq, timer = Tref} = State) ->
   erlang:cancel_timer(Tref),
   receive check -> ok after 0 -> ok end,
   State#fsm{
      timer = erlang:send_after(Freq, self(), check)
   }.

%%
%% check if sensor is permanently failed
is_failed(#fsm{t = undefined} = State) ->
   {false, State};

is_failed(#fsm{failure = F, t = T, r = R} = State) ->
   T0 = os:timestamp(),
   T1 = tempus:sub(T0, T),
   L  = lists:filter(fun(X) -> X >= T1 end, F),
   case length(L) of
      N when N >= R ->
         {true,  State#fsm{failure = []}};
      _ ->
         {false, State#fsm{failure = [T0 | L]}}
   end.

%%
%% check sensor key 
is_key_valid(#fsm{mod = {Mod, Fun}, key = Key, safety = Safety}) ->
   valid(Safety, Mod:Fun(Key));
is_key_valid(#fsm{mod = {Mod, Fun, Fa}, key = Key, safety = Safety}) ->
   valid(Safety, Mod:Fun(Fa, Key)).

valid(is, undefined) ->
   false;
valid(is, _) ->
   true;
valid(_, undefined) ->
   undefined;
valid({eq, A}, X) ->
   X =:= A;
valid({ne, A}, X) ->
   X =/= A;
valid({gt, A}, X) ->
   X > A;
valid({ge, A}, X) ->
   X >= A;
valid({lt, A}, X) ->
   X < A;
valid({le, A}, X) ->
   X =< A.

%%
%% update system status
update(#fsm{mod = {clue, _}, key = Key}, Val) ->
   ets:insert(health, {Key, Val});

update(#fsm{key = Key}, Val) ->
   ets:insert(health, {Key, Val}).

   
