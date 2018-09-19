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
- Add ability to get Order Book state for any moment in the past.
- Add CI builds via wercker or travis.
- Add REST interface to the application.
- Dockerize.
