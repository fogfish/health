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
%%    tcp/ip socket acceptor  
-module(health_accept).
-behaviour(pipe).

-export([
   start_link/0
  ,init/1
  ,free/2
  ,ioctl/2
  ,'LISTEN'/3
]).

%%
%% tcp socket options
-define(SO_TCP_POOL,    10).
-define(SO_TCP, [
   binary
  ,{nodelay,    true}
  ,{keepalive,  true}
  ,{recbuf,     256 * 1024}
  ,{sndbuf,     128 * 1024}
]).

%% internal state
-record(fsm, {
   sock   = undefined :: port()
}).

%%%----------------------------------------------------------------------------   
%%%
%%% Factory
%%%
%%%----------------------------------------------------------------------------   

%%
%%
start_link() ->
   pipe:start_link(?MODULE, [], []).

init([]) ->
   case opts:val(http, undefined, health) of
      undefined ->
         ok;
      Port      ->
         pipe:send(self(), {listen, Port, []})
   end,
   {ok, 'LISTEN', #fsm{}}.

free(_, #fsm{sock = Sock}) ->
   (catch gen_tcp:close(Sock)),
   ok.

%%
%% ioctl
ioctl(socket, #fsm{sock = Sock}) ->
   Sock.

%%%----------------------------------------------------------------------------   
%%%
%%% fsm
%%%
%%%----------------------------------------------------------------------------   

'LISTEN'({listen, Port, Opts}, _Tx, State) ->
   case gen_tcp:listen(Port, [{active, false},{reuseaddr, true}|?SO_TCP]) of
      {ok, Sock} -> 
         ok  = lists:foreach(
            fun(_) ->
               {ok, _} = supervisor:start_child(health_socket_sup, [{accept, Sock, Opts}])
            end,
            lists:seq(1, ?SO_TCP_POOL)
         ),
         {next_state, 'LISTEN', State#fsm{sock=Sock}};
      {error, Reason} ->
         {stop, Reason, State}
   end;

'LISTEN'(_Msg, _, State) ->
   {next_state, 'LISTEN', State}.

