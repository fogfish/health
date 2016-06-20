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
%%   health monitor application
-module(health).

-export([start/0]).
-export([
   start_link/1,
   new/1,
   check/1
]).

-type(key()      :: any()).
-type(safety()   :: {ops(), number()}).
-type(t()        :: integer()).
-type(n()        :: integer()).
-type(s()        :: integer()).
-type(ops()      :: '>' | '<' | '=' | '=:=' | '>=' | '=<' ).
-type(strategy() :: {check, t()} | {supervise, t(), n(), s()} | {lens, t(), _}).
-type(spec()     :: {key(), safety(), strategy()}).

%%
%%
start() ->
   applib:boot(?MODULE, []).

%%
%% start health sensor process.   
%%
%% the sensor process is responsible for poll and monitor a key from 
%% system status repository. The sensor will break if key's value do not met
%% safety conditions. 
%%
%% Each sensor is defined via sensor specification:
%%    {key(), safety(), strategy()}
%%
%% The following strategies are supported
%%
%%   * {check, t()} - evaluate safety margin every t() millisecond
%%
%%   * {supervise, t(), n(), s()} - evaluate safety margin every t() millisecond.
%%     the sensor terminates itself, if it exceed failure frequency (more than n()
%%     failure within s() seconds). 
%% 
%%   * {lens, t(), _} - focus the sensor to value and apply custom function
%%
%% Each application can uses health:start_link(...) sensors within its own supervisors to
%% handle resource failure. Alternatively, sensors can be installed into health application
%% using config file or explicitly via new function:
%%
%%    {health, [
%%       {sensors, [
%%          ...
%%       ]}
%%    ]}   
-spec(start_link/1 :: (spec()) -> {ok, pid()} | {error, any()}).

start_link(Spec) ->
   health_sensor:start_link(Spec).


-spec(new/1 :: (spec()) -> {ok, pid()} | {error, any()}).

new(Spec) ->
   supervisor:start_child(health_sensor_sup, [Spec]).

%%
%% check health status
-spec(check/1 :: (any()) -> ok | failed | undefined).

check(Key) ->
   case ets:lookup(health, Key) of
      []         -> undefined;
      [{_, Val}] -> Val
   end.
