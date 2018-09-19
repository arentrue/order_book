-module(order_book_instrument).

-export([new/2]).

%%

-type id() :: _.

-type init_args() :: #{
    aggregate := order_book_aggregate:config()
}.

-spec new(id(), init_args()) ->
    {ok, pid()} | {error, {present, pid()}}.
new(ID, InitArgs) ->
    case supervisor:start_child(order_book_instruments_sup:get_ref(), [ID, InitArgs]) of
        Ok = {ok, _Pid}                 -> Ok;
        {error, {already_started, Pid}} -> {error, {present, Pid}}
    end.
