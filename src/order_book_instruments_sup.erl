-module(order_book_instruments_sup).

%% API
-export([start_link/1]).
-export([get_ref/0]).

%% Supervisor

-behaviour(supervisor).

-export([init/1]).

%%

-type config()   :: _.

-spec start_link(config()) ->
    {ok, pid()} | {error, {already_started, pid()}}.

start_link(Config) ->
    supervisor:start_link(get_ref(), ?MODULE, Config).

-spec get_ref() ->
    order_book_reg:ref().
get_ref() ->
    order_book_reg:get_ref({sup, ?MODULE}).

%%

-spec init(config()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init(Config) ->
    {ok, {
        #{
            strategy  => simple_one_for_one
        },
        [
            #{
                id      => order_book_instrument_sup,
                start   => {order_book_instrument_sup, start_link, [Config]},
                type    => supervisor,
                restart => transient
            }
        ]
    }}.
