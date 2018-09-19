-module(order_book).

%% API

-export([new_instrument/2]).
-export([new_exchange/3]).
-export([add_order/5]).
-export([modify_order/3]).
-export([delete_order/2]).
-export([get_book/1]).

-type instrument() :: order_book_instrument:id().
-type exchange()   :: order_book_exchange:id().
-type instr_args() :: order_book_instrument:init_args().
-type exch_args()  :: order_book_exchange:init_args().

-type order_id()    :: order_book_order:id().
-type order_type()  :: order_book_order:type().
-type order_price() :: order_book_order:price().
-type order_qty()   :: order_book_order:qty().
-type book()        :: order_book_instrument:book().

-export_type([instrument/0]).
-export_type([exchange/0]).
-export_type([instr_args/0]).
-export_type([exch_args/0]).

-export_type([order_id/0]).
-export_type([order_type/0]).
-export_type([order_price/0]).
-export_type([order_qty/0]).
-export_type([book/0]).

-spec new_instrument(instrument(), instr_args()) ->
    {ok, pid()} | {error, {present, pid()}}.
new_instrument(ID, InitArgs) ->
    order_book_instrument:new(ID, InitArgs).

-spec new_exchange(exchange(), instrument(), exch_args()) ->
    {ok, pid()} | {error, {present, pid()}}.
new_exchange(Exchange, Instrument, InitArgs) ->
    order_book_exchange:new(Exchange, Instrument, InitArgs).

-spec add_order(exchange(), order_id(), order_type(), order_price(), order_qty()) ->
    ok | {error, duplicate}.
add_order(ID, OrderID, Type, Price, Qty) ->
    order_book_exchange:add_order(ID, OrderID, Type, Price, Qty).

-spec delete_order(exchange(), order_id()) ->
    ok | {error, deleted | notfound}.
delete_order(ID, OrderID) ->
    order_book_exchange:delete_order(ID, OrderID).

-spec modify_order(exchange(), order_id(), order_qty()) ->
    ok | {error, deleted | notfound}.
modify_order(ID, OrderID, NewQty) ->
    order_book_exchange:modify_order(ID, OrderID, NewQty).

-spec get_book(instrument()) ->
  book().
get_book(ID) ->
    order_book_instrument:get_book(ID).
