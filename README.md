# Order Book

An aggregated Order Book in Erlang, which maintains total _Qtys_ for each _Price_ level for _bid_ and _ask_ sides. The events coming (somehow) from an Exchange:

```erlang
{new_order, OrderID :: integer(), bid | ask, Price :: number(), Qty :: integer()} ,
{delete_order, OrderID :: integer()} ,
{modify_order, OrderID :: integer(), NewQty :: integer() }
```

> Note: implemented in Erlang 21.

## Hints

- compile: `make compile`

- dialyze `make dialyze`

- xref `make xref`

- lint (requires elvis): `make lint`

- test `make test`
