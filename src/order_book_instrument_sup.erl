-module(order_book_instrument_sup).

%% API
-export([start_link/3]).

%% Supervisor

-behaviour(supervisor).

-export([init/1]).

-type id()        :: order_book_instrument:id().
-type init_args() :: order_book_instrument:init_args().
-type config()    :: order_book_instruments_sup:config().

%%

-spec start_link(config(), id(), init_args()) ->
    {ok, pid()} | {error, {already_started, pid()}}.

start_link(_Config, ID, InitArgs) ->
    supervisor:start_link(get_ref(ID), ?MODULE, {ID, InitArgs}).

get_ref(ID) ->
    order_book_reg:get_ref({?MODULE, {sup, ID}}).
%%

-spec init({id(), init_args()}) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init({ID, #{instrument := Config}}) ->
    {ok, {
        #{strategy => rest_for_one},
        [
            #{
                id      => order_book_instrument,
                start   => {order_book_instrument, start_link, [ID, Config]},
                type    => worker,
                restart => transient
            },
            order_book_exchange:child_spec(ID)
        ]
    }}.
