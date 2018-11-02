-module(order_book_exchange).

%% TODO
%%   - Add a table manager process which will be a creator and a heir
%%     for all the ets tables. This will allow to keep states between
%%     gen server restarts.
%%
%%   - Add get_orders/1, get_order/2 api functions similar
%%     to order_book_instrument:get_book/1.
%%
%%   - Add support for batch requests (e.g. init a table with a batch).
%%

%% API
-export([child_spec/1]).
-export([new/3]).
-export([add_order/5]).
-export([delete_order/2]).
-export([modify_order/3]).

%%

-type id()          :: order_book_server:id(exchange()).
-type exchange()    :: term().
-type instrument()  :: order_book_instrument:id().
-type store_id()    :: atom().
-type init_args() :: #{
    store := store_id()
}.

-export_type([id/0]).
-export_type([instrument/0]).
-export_type([exchange/0]).
-export_type([store_id/0]).
-export_type([init_args/0]).

-type order()       :: order_book_order:order().
-type order_id()    :: order_book_order:id().
-type order_type()  :: order_book_order:type().
-type order_price() :: order_book_order:price().
-type order_qty()   :: order_book_order:qty().

-type order_req() ::
    {add_order,    order()}                   |
    {delete_order, order_id()}                |
    {modify_order, {order_id(), order_qty()}} .

-type config()   :: #{
    instrument := instrument()
}.

% order_book_server behaviour
-behaviour(order_book_server).

-export([init_start/2]).
-export([init_finish/2]).
-export([is_supported/2]).
-export([handle_call/3]).

%% API

-spec child_spec(instrument()) ->
    supervisor:child_spec().
child_spec(Instrument) ->
    order_book_server:child_spec(Instrument, ?MODULE, #{instrument => Instrument}).

-spec new(exchange(), instrument(), init_args()) ->
    {ok, pid()} | {error, {present, pid()}}.
new(Exchange, Instrument, InitArgs) ->
    order_book_server:start(Exchange, Instrument, InitArgs, ?MODULE).

-spec add_order(id(), order_id(), order_type(), order_price(), order_qty()) ->
    ok | {error, duplicate}.
add_order(ID, OrderID, Type, Price, Qty) ->
    Order = order_book_order:new(OrderID, Type, Price, Qty),
    order_book_server:call(ID, {add_order, Order}).

-spec delete_order(id(), order_id()) ->
    ok | {error, deleted | notfound}.
delete_order(ID, OrderID) when is_integer(OrderID) ->
    order_book_server:call(ID, {delete_order, OrderID}).

-spec modify_order(id(), order_id(), order_qty()) ->
    ok | {error, deleted | notfound}.
modify_order(ID, OrderID, NewQty) when
    is_integer(OrderID)                    andalso
    is_integer(NewQty) andalso NewQty >= 0
->
    order_book_server:call(ID, {modify_order, {OrderID, NewQty}}).

%% order_book_server behaviour

-type st() :: order_book_server:handler_st(#{
    id         := id(),
    instrument := instrument(),
    store      => store_id()
}).

-spec init_start(id(), config()) ->
   st().

init_start(ID, #{instrument := Instrument}) ->
    #{
        id         => ID,
        instrument => Instrument
    }.

-spec init_finish(init_args(), st()) ->
   st().
init_finish(#{store := Store}, St) ->
    case ets:info(Store) of
        undefined ->
            Store = ets:new(Store, [ordered_set, protected, named_table, {keypos, 1}]);
        _ ->
            erlang:error({store_exists, Store})
    end,
    St#{store => Store}.

-spec is_supported(order_book_server:req_type(), order_book_server:action()) ->
    boolean().
is_supported(call, A) when
      A =:= add_order ; A =:= delete_order ; A =:= modify_order
->
    true;
is_supported(_, _) ->
    false.

-spec handle_call(order_req(), st(), order_book_server:caller()) ->
    st() | {reply, {error, duplicate | deleted | notfound}, st()}.
handle_call({add_order, Order}, St = #{id := ID, store := Store}, From) ->
    case ets:insert_new(Store, order_book_order:to_tuple(Order)) of
        true ->
            finish_update({add_order, Order}, From, St);
        false ->
            ok = logger:warning("Exchange[~p]: attempt to add existing order: ~p", [ID, Order]),
            {reply, {error, duplicate}, St}
    end;
handle_call({delete_order, OrderID}, St = #{id := ID, store := Store}, From) ->
    case ets:lookup(Store, OrderID) of
        [{OrderID, deleted}] ->
            ok = logger:warning("Exchange[~p]: attempt to delete already deleted order: ~p", [ID, OrderID]),
            {reply, {error, deleted}, St};
        [Order] ->
            true = ets:insert(Store, {OrderID, deleted}),
            finish_update({delete_order, order_book_order:from_tuple(Order)}, From, St);
        [] ->
            ok = logger:warning("Exchange[~p]: attempt to delete non existing order: ~p", [ID, OrderID]),
            {reply, {error, notfound}, St}
    end;
handle_call({modify_order, {OrderID, NewQty}}, St = #{id := ID, store := Store}, From) ->
    case ets:lookup(Store, OrderID) of
        [{OrderID, deleted}] ->
            ok = logger:warning("Exchange[~p]: attempt to modify deleted order: ~p", [ID, OrderID]),
            {reply, {error, deleted}, St};
        [Order0] ->
            Order = order_book_order:from_tuple(Order0),
            Modified = order_book_order:to_tuple(order_book_order:modify(NewQty, Order)),
            true = ets:insert(Store, Modified),
            finish_update({modify_order, {Order, NewQty}}, From, St);
        [] ->
            ok = logger:warning("Exchange[~p]: attempt to modify non existing order: ~p", [ID, OrderID]),
            {reply, {error, notfound}, St}
    end.

finish_update(Update, From, St = #{id := ID, instrument := Instrument}) ->
    ok = order_book_server:reply(From, ok),
    ok = update_instrument(Update, Instrument, ID),
    St.

update_instrument({add_order, Order}, Instrument, Exchange) ->
    order_book_instrument:update(Instrument, Order, Exchange);
update_instrument({delete_order, Order}, Instrument, Exchange) ->
    Qty = order_book_order:qty(Order),
    order_book_instrument:update(Instrument, order_book_order:modify(-Qty, Order), Exchange);
update_instrument({modify_order, {Order, NewQty}}, Instrument, Exchange) ->
    Qty = order_book_order:qty(Order),
    order_book_instrument:update(Instrument, order_book_order:modify(NewQty - Qty, Order), Exchange).
