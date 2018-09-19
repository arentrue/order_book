-module(order_book_aggregate).

%% API
-export([start_link/2]).
-export([update/3]).
-export([read/1]).

%%

-type id() :: order_book_instrument:id().

-type store_id() :: atom().
-type config()   :: #{
    store := store_id()
}.

-export_type([id/0]).
-export_type([store_id/0]).
-export_type([config/0]).

-type order() :: order_book_order:order().

-type exchange() :: term().

% order_book_server behaviour
-behaviour(order_book_server).

-export([init_start/2]).
-export([init_finish/2]).
-export([is_supported/2]).
-export([handle_cast/2]).

-define(STORE_KEY(ID), {p, l, ID}).

%% API

-spec start_link(id(), config()) ->
    {ok, pid()} | {error, {already_started, pid()}}.
start_link(ID, Config) ->
    order_book_server:start_link(?MODULE, Config, ID, undefined).

-spec update(id(), order(), exchange()) ->
    ok.
update(ID, Order, Exchange) ->
    order_book_server:cast(ID, {update, {Order, Exchange}}).

-spec read(id()) ->
  list().
read(ID) ->
    Store = gproc:get_value(?STORE_KEY(ID), get_pid(ID)),
    ets:tab2list(Store).

get_pid(ID) ->
    {via, gproc, Key} = order_book_server:get_ref(server, ID),
    gproc:lookup_pid(Key).

%% order_book_server behaviour

-type st() :: order_book_server:handler_st(#{
    id     := id(),
    store  := store_id()
}).

-type req() :: {update, {order(), exchange()}}.

-spec init_start(id(), config()) ->
   st().

init_start(ID, #{store := Store}) ->
    #{
        id    => ID,
        store => Store
    }.

-spec is_supported(order_book_server:req_type(), order_book_server:action()) ->
    boolean().
is_supported(cast, update) -> true;
is_supported(_, _)         -> false.

-spec init_finish(_, st()) ->
   st() | no_return().
init_finish(_, St = #{store := Store, id := ID}) ->
    case ets:info(Store) of
        undefined ->
            Store = ets:new(Store, [ordered_set, protected, named_table, {keypos, 1}]),
            ok = reg_store(ID, Store),
            Store;
        _ ->
            erlang:error({store_exists, Store})
    end,
    St.

reg_store(ID, Store) ->
    true = gproc:reg(?STORE_KEY(ID), Store),
    ok.

-spec handle_cast(req(), st()) ->
    st().
handle_cast({update, {Order, Exchange}}, St = #{id := ID, store := Store}) ->
    Price   = order_book_order:price(Order),
    QtyDiff = order_book_order:qty(Order),
    Type    = order_book_order:type(Order),
    Trace   = {ID, order_book_order:id(Order), Exchange},
    _ = case ets:lookup(Store, Price) of
        [] ->
            {Bid, Ask} = new_entry(Type, QtyDiff, Trace),
            true = ets:insert(Store, {Price, Bid, Ask});
        [{P, Bid, Ask}] ->
            {Bid1, Ask1} = update_values(Type, QtyDiff, Bid, Ask, Trace),
            true = ets:insert(Store, {P, Bid1, Ask1})
    end,
    St.

new_entry(Type, Qty, Trace) ->
    update_values(Type, Qty, 0, 0, Trace).

update_values(Type, Diff, Bid, Ask, {ID, OrderID, Exchange}) ->
    case update_values(Type, Diff, Bid, Ask) of
        {ok, V} ->
            V;
        {error, V} ->
            ok = lager:warning(
                "Aggregate[~p]: negative ~p value from exchange ~p order: ~p; value before: ~p",
                [ID, Type, Exchange, OrderID, get_val(Type, Bid, Ask)]
            ),
            V
    end.

update_values(bid, Diff, Bid, Ask) ->
    {Status, Bid1} = update_qty(Bid, Diff),
    {Status, {Bid1, Ask}};
update_values(ask, Diff, Bid, Ask) ->
    {Status, Ask1} = update_qty(Ask, Diff),
    {Status, {Bid, Ask1}}.

update_qty(Base, Diff) when Base + Diff < 0 ->
    {error, 0};
update_qty(Base, Diff) ->
    {ok, Base + Diff}.

get_val(bid, Bid, _Ask) -> Bid;
get_val(ask, _Bid, Ask) -> Ask.
