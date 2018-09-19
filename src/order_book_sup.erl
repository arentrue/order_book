%%%-------------------------------------------------------------------
%% @doc order_book top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(order_book_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() ->
    {ok, pid()} | {error, any()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(_) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    Config = undefined,
    {ok, {
        #{
            strategy  => one_for_one
        },
        [
            #{
                id      => order_book_instruments_sup,
                start   => {order_book_instruments_sup, start_link, [Config]},
                type    => supervisor,
                restart => permanent
            }
        ]
    }}.

%%====================================================================
%% Internal functions
%%====================================================================
