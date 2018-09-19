-module(order_book_reg).

-export([get_ref/1]).

-type ref() :: {via, gproc, gproc:key()}.
-export_type([ref/0]).

-spec get_ref(any()) ->
    ref().
get_ref(Name) ->
    {via, gproc, get_gproc_ref(Name)}.

get_gproc_ref(Name) ->
    {n, l, Name}.

