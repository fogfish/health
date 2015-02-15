%% @description
%%   
-module(health_socket).
-behaviour(pipe).

-export([
   start_link/1
  ,init/1
  ,free/2
  ,ioctl/2
  ,'IDLE'/3
  ,'ACTIVE'/3
]).

%% internal state
-record(fsm, {
   sock = undefined :: port() %% tcp/ip socket
  ,http = undefined :: any()  %% http packet stream
}).

-define(SO_TCP_ACTIVE, 100).

%%%----------------------------------------------------------------------------   
%%%
%%% Factory
%%%
%%%----------------------------------------------------------------------------   

%%
%%
start_link(Msg) ->
   pipe:start_link(?MODULE, [Msg], []).

init([{_, _, _}=Msg]) ->
   pipe:send(self(), Msg),
   {ok, 'IDLE', #fsm{}}.

free(_, #fsm{sock = Sock}) ->
   (catch gen_tcp:close(Sock)),
   ok.

%%
%% ioctl
ioctl(_, _State) ->
   throw(not_supported).

%%%----------------------------------------------------------------------------   
%%%
%%% fsm
%%%
%%%----------------------------------------------------------------------------   

%%
%%
'IDLE'({accept, LSock, Opts}, _Tx, State) ->
   case gen_tcp:accept(LSock) of
      %% socket is accepted
      {ok, Sock} ->
         {ok, _} = supervisor:start_child(health_socket_sup, [{accept, LSock, Opts}]),
         inet:setopts(Sock, [{active, once}]),      
         {next_state, 'ACTIVE', 
            State#fsm{
               sock = Sock
              ,http = htstream:new()
            }
         };

      %% listen socket is terminated
      {error, closed} ->
         {stop, normal, State};

      %% 
      {error, Reason} ->
         {ok, _} = supervisor:start_child(health_socket_sup, [{accept, LSock, Opts}]),      
         {stop, Reason, State}
   end.

%%
%%
'ACTIVE'(close, _Tx, S) ->
   {stop, normal, S};

'ACTIVE'({tcp, _, Pckt}, _Tx, #fsm{sock=Sock, http=Http0}=State) ->
   inet:setopts(Sock, [{active, once}]),
   case request(Pckt, Sock, Http0) of
      {true,  Http1} ->
         {next_state, 'ACTIVE', State#fsm{http = Http1}};
      {false, Http1} ->
         {stop, normal, State#fsm{http = Http1}}
   end;

'ACTIVE'({tcp_error,  _, Reason}, _Tx, State) ->
   {stop, Reason, State};

'ACTIVE'({tcp_closed, _}, _Tx, State)  ->
   {stop, normal, State};

'ACTIVE'(_, _Tx, State) ->
   {next_state, 'ACTIVE', State}.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

request(Pckt, Sock, Stream0) ->
   case htstream:decode(Pckt, Stream0) of
      {{'GET', <<$/, Key/binary>>, _}, Stream1} ->
         response(Sock, check_health(Key)),
         {true, Stream1};

      {{_, _, _}, Stream1} ->
         response(badarg, Sock),
         {false, Stream1};

      {[], Stream1} ->
         {true,  Stream1}
   end.

check_health(Req) ->
   health:check(req_to_key(Req)).

req_to_key(Req) ->
   case binary:split(Req, <<$.>>, [global, trim]) of
      [Key] ->
         scalar:a(Key);
       Key  ->
         erlang:list_to_tuple([scalar:a(X) || X <- Key])
   end.

response(Sock,   Code) ->
   {Http, _} = htstream:encode({
      Code, 
      [
         {'Connection',     <<"keep-alive">>},
         {'Content-Length',  0}
      ]
   }),
   gen_tcp:send(Sock, Http).






