# Order Book

An aggregated Order Book in Erlang, which maintains total _Qtys_ for each _Price level_ for _bid_ and _ask_ sides. The events coming (somehow) from an exchange:

```erlang
{new_order, OrderID :: integer(), bid | ask, Price :: number(), Qty :: integer()} ,
{delete_order, OrderID :: integer()} ,
{modify_order, OrderID :: integer(), NewQty :: integer() }
```

It is assumed, that the exchange is responsible for matching orders and the results are reflected in the events to the Order Book (above).

## Hints

> Note: implemented in Erlang 21.

- compile: `make compile`

- dialyze `make dialyze`

- xref `make xref`

- lint (requires elvis): `make lint`

- test `make test`

- release `make release`

## TODO

- Add more sophisticated tests with a load profile.
- Add CI builds via wercker or travis.
- Add REST interface to the application.
- Dockerize.

### Provide Order Book state for any timestamp

__Implementation sketch__

1. _order_book_exchange_: add a table:

    ```erlang
    Updates :: {Timestamp :: unixtime(), Update :: {Price :: number, Type :: bid | ask, QtyDiff :: integer()}}.
    ```

    with entry for each order. Send the `Timestamp` to _order_book_instrument_ together with exchange id and update data via _update_instrument_ function.

2. _order_book_instrument_: add a map to the state:

    ```erlang
    LastUpdates :: #{Exchange :: order_book_exchange:id() => Timestamp}.
    ```

    containing timestamps received in the last update from each _order_book_exchange_ server.

    Periodically cache current state of the order book table to a table:

    ```erlang
    Cache :: {Timestamp, CacheTabID, LastUpdates}
    ```

    1. Create a new entry in the `Cache` table  with the current `Timestamp`, `LastUpdates` and `CacheTabID` = `undefined`.
    2. Fork a new process, which will create a copy of the current order book table and [give_away](http://erlang.org/doc/man/ets.html#give_away-3) back to the _order_book_instrument_ server when finished.
    3. Until ownership transfer for the new table is not received, store all the coming updates to a temporary table.
    4. On receiveing the message apply the updates from the temporaty table to the main one; update the last entry in `Cache` table with received `CacheTabID`.
    5. Remove the temporaty table.

    Update existing `get_book/1` and add a new api function `get_book/2`:

    ```erlang
    -spec get_book(id()) ->
        book().
    get_book(ID) ->
        MainTabID = get_tab_id(book, ID),
        Book = ets:tab2list(MainTabID),
        UpdatesTab = get_tab_id(updates, ID)
        case ets:info(UpdatesTab) of
            undefined -> Book;
            _         -> apply_updates(UpdatesTab, Book)
        end.

    -spec get_book(id(), unixtime()) ->
        book().
    get_book(ID, Timestamp) ->
        Tab = get_tab_id(cache, ID),
        {InstrTimestamp, CacheTabID, LastUpdates} = find_proximate(Timestamp, Tab),
        Direction = get_direction(InstrTimestamp, Timestamp),
        Cache = ets:tab2list(CacheTabID),
        maps:fold(
            fun({Exch, ExchTimestamp}, Acc) ->
                UpdTab = order_book_exchange:get_tab_id(updates, Exch),
                apply_exchange_updates(Direction, ExchTimestamp, Timestamp, UpdTab, Acc)
            end,
            Cache,
            LastUpdates
        ).
    ```

3. _order_book_: add an interface api function for the `order_book_instrument:get_book/2` above.
