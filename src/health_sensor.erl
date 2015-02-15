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
   key     = undefined :: any(),      %% key to poll
   safety  = undefined :: float(),    %% expected safety range
   freq    = undefined :: integer(),  %% frequency to poll value
   r       = undefined :: integer(),  %% max failures (r) occurs within (t) seconds
   t       = undefined :: integer(),  %% 
   failure = []        :: list()      %% log of recent failures 
}).

%%%----------------------------------------------------------------------------   
%%%
%%% factory
%%%
%%%----------------------------------------------------------------------------   

start_link(Spec) ->
   pipe:start_link(?MODULE, [Spec], []).

init([{Key, Safety, Strategy}]) ->
   erlang:send(self(), check),
   {ok, active, 
      strategy(Strategy,
         #fsm{
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
active(check, _, #fsm{key = Key}=State0) ->
   case is_key_valid(State0) of
      true  ->
         {next_state,  active, timeout(State0)};

      false ->
         ets:insert(health, {Key, not_available}),
         case 
            is_failed(State0)
         of
            {true,  State1} ->
               {stop, {unhealth, Key, clue:get(Key)}, State1};
            {false, State1} ->
               {next_state, broken, timeout(State1)}
         end
   end;

active(_, _, State) ->
   {next_state, broken, State}.

%%
%%
broken(check, _, #fsm{key = Key}=State) ->
   case is_key_valid(State) of
      true  ->
         ets:insert(health, {Key, ok}),
         {next_state,  active, timeout(State)};

      false ->
         {next_state, broken, timeout(State)}
   end;

broken(_, _, State) ->
   {next_state, broken, State}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% define sensor strategy
strategy({check, T}, State) ->
   State#fsm{freq = T};

strategy({supervise, T, MaxR, MaxT}, State) ->
   State#fsm{freq = T, r = MaxR, t = MaxT}.

%%
%%
timeout(#fsm{freq = Freq} = State) ->
   erlang:send_after(Freq, self(), check),
   State.

%%
%%  check if sensor is permanently failed
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
is_key_valid(#fsm{key = Key, safety = {A, B}}) ->
   case clue:get(Key) of
      undefined ->
         false;
      X ->
         X >= A andalso X < B
   end;

is_key_valid(#fsm{key = Key, safety = B}) ->
   case clue:get(Key) of
      undefined ->
         false;
      X ->
         X < B
   end.

