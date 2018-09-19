-module(order_book_order).

%% TODO
%%   - Change Order from map to record here: it's way more handy with ets.
%%

%% API

-export([id/1]).
-export([type/1]).
-export([price/1]).
-export([qty/1]).

-export([new/4]).
-export([from_tuple/1]).
-export([to_tuple/1]).
-export([modify/2]).

%%

-type id()    :: integer().
-type type()  :: ask | bid.
-type price() :: number().
-type qty()   :: integer().

-type order() :: #{
    id    := id(),
    type  := type(),
    price := price(),
    qty   := qty()
}.

-export_type([id/0]).
-export_type([type/0]).
-export_type([price/0]).
-export_type([qty/0]).
-export_type([order/0]).

-spec id(order()) -> id().
id(#{id := V}) -> V.

-spec type(order()) -> type().
type(#{type := V}) -> V.

-spec price(order()) -> price().
price(#{price := V}) -> V.

-spec qty(order()) -> qty().
qty(#{qty := V}) -> V.

-spec new(id(), type(), price(), qty()) ->
    order() | no_return().
new(ID, Type, Price, Qty) ->
    from_tuple({ID, Type, Price, Qty}).

-spec from_tuple({id(), type(), price(), qty()}) ->
    order() | no_return().
from_tuple({ID, Type, Price, Qty}) when
    is_integer(ID)                      andalso
    (Type =:= ask orelse Type =:= bid)  andalso
    is_number(Price) andalso Price >= 0 andalso
    is_integer(Qty)
->
    #{
        id    => ID,
        type  => Type,
        price => Price,
        qty   => Qty
    };
from_tuple(T) ->
    erlang:error(badarg, T).

-spec to_tuple(order()) ->
    {id(), type(), price(), qty()}.
to_tuple(#{id := ID, type := Type, price := Price, qty := Qty}) ->
    {ID, Type, Price, Qty}.

-spec modify(qty(), order()) ->
    order().
modify(NewQty, Order) when
    is_integer(NewQty)
->
    Order#{qty => NewQty}.
