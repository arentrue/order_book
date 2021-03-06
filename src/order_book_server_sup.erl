-module(order_book_server_sup).

%% TODO
%%   - Change it to order_book_exchange_sup and move all child_spec and supervisor:start_child
%%     related staff from order_book_server to order_book_exchange.
%%

%% API
-export([start_link/2]).

%% Supervisor

-behaviour(supervisor).

-export([init/1]).

%%

-type sup_name() :: _.
-type mfargs()   :: {module(), function(), list()}.

-spec start_link(sup_name(), mfargs()) ->
    {ok, pid()} | {error, {already_started, pid()}}.

start_link(SupName, MFA) ->
    supervisor:start_link(SupName, ?MODULE, MFA).

%%

-spec init(mfargs()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(MFA = {Module, _, _}) ->
    {ok, {
        #{strategy => simple_one_for_one},
        [
            #{
                id      => Module,
                start   => MFA,
                type    => worker,
                restart => transient
            }
        ]
    }}.

