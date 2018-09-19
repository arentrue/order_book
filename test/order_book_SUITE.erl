-module(order_book_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([book_test/1]).

-type test_name() :: atom().
-type config() :: [{atom(), _}].

-spec all() ->
    [test_name()].
all() ->
    [
        book_test
    ].

-spec init_per_suite(config()) ->
    config().
init_per_suite(C) ->
    {ok, Apps} = application:ensure_all_started(order_book),
    [{apps, Apps} | C].

-spec end_per_suite(config()) ->
    any().
end_per_suite(C) ->
    [application_stop(App) || App <- proplists:get_value(apps, C)].

application_stop(App) ->
    application:stop(App).

%%
%% tests
%%
-spec book_test(config()) ->
    any().
book_test(_C) ->
    E = <<"NYSE">>,
    I = <<"btc_usd">>,
    {ok, _} = order_book_instrument:new(I, #{aggregate => #{store => btc_usd}}),
    {ok, _} = order_book_exchange:new(E, I, #{store => nyce}),

    ok = add(E, 1, ask, 6354.2, 10),
    [{6354.2, 0, 10}] = read(I),

    ok = add(E, 2, bid, 6354.005, 20),
    [{6354.005, 20, 0}, {6354.2, 0, 10}] = read(I),

    ok = add(E, 3, ask, 6354.87, 20),
    [{6354.005, 20, 0}, {6354.2, 0, 10}, {6354.87, 0, 20}] = read(I),

    ok = add(E, 4, bid, 6354.005, 50),
    [{6354.005, 70, 0}, {6354.2, 0, 10}, {6354.87, 0, 20}] = read(I),

    ok = mod(E, 1, 50),
    [{6354.005, 70, 0}, {6354.2, 0, 50}, {6354.87, 0, 20}] = read(I),

    ok = mod(E, 2, 5),
    [{6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 20}] = read(I),

    ok = del(E, 3),
    [{6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 0}] = read(I),

    {error, deleted} = del(E, 3),
    [{6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 0}] = read(I),

    {error, notfound} = del(E, 5),
    [{6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 0}] = read(I),

    {error, duplicate} = add(E, 1, ask, 6354.01, 10),
    [{6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 0}] = read(I),

    {error, duplicate} = add(E, 3, bid, 6354.6, 10),
    [{6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 0}] = read(I),

    {error, deleted} = mod(E, 3, 100),
    [{6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 0}] = read(I),

    {error, notfound} = mod(E, 5, 3),
    [{6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 0}] = read(I),

    ok = add(E, 5, ask, 6354, 25),
    [{6354, 0, 25}, {6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 0}] = read(I),

    ok = add(E, 6, ask, 6354.0, 15),
    [{6354, 0, 40}, {6354.005, 55, 0}, {6354.2, 0, 50}, {6354.87, 0, 0}] = read(I).

%%

add(E, I, T, P, Q) ->
    order_book_exchange:add_order(E, I, T, P, Q).
mod(E, I, Q) ->
    order_book_exchange:modify_order(E, I, Q).
del(E, I) ->
    order_book_exchange:delete_order(E, I).

read(A) ->
    read(A, 5).

read(A, T) ->
    %% This is ugly indeed. Even retries would look better here.
    timer:sleep(T),
    order_book_aggregate:read(A).
