%%
%%
-module(health_sensor_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 5000, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 5000, Type, dynamic}).

%%
%%
start_link() ->
   {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
   ok = lists:foreach(
      fun(Spec) ->
         {ok, _} = supervisor:start_child(Sup, [Spec])
      end,
      opts:val(sensors, [], health)
   ),
   {ok, Sup}.
   
init([]) -> 
   {ok,
      {
         {simple_one_for_one, 0, 1},
         [
            ?CHILD(worker, health_sensor, [])
         ]
      }
   }.
