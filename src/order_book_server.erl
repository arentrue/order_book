-module(order_book_server).

%% API
-export([child_spec/3]).
-export([start_link/4]).
-export([start/4]).
-export([call/2]).
-export([call/3]).
-export([cast/2]).
-export([reply/2]).

-export([get_ref/2]).

%% Behaviour definition
-type req_type()    :: call | cast | info.
-type action()      :: atom().
-type req(T)        :: {action(), T}.
-type handler_st(T) :: T.
-type config(T)     :: T.
-type id(T)         :: T.
-type sup_id(T)     :: id(T).
-type init_args(T)  :: T.
-opaque caller()    :: {pid(), reference()}.

-export_type([req_type/0]).
-export_type([req/1]).
-export_type([action/0]).
-export_type([handler_st/1]).
-export_type([config/1]).
-export_type([init_args/1]).
-export_type([caller/0]).

-callback init_start(id(_), config(_)) ->
    handler_st(_).

-callback init_finish(init_args(_), handler_st(_)) ->
    handler_st(_).

-callback is_supported(req_type(), action()) ->
    boolean().

-callback handle_call(req(_), handler_st(_), caller()) ->
    handler_st(_) | {reply, _Reply, handler_st(_)}.

-callback handle_cast(req(_), handler_st(_)) ->
    handler_st(_).

-callback handle_info(req(_), handler_st(_)) ->
    handler_st(_).

-optional_callbacks([handle_call/3, handle_cast/2, handle_info/2]).

%% gen_server behaviour
-behaviour(gen_server).

-export([init/1]).
-export([handle_continue/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type handler() :: module().
-export_type([handler/0]).

%% API

-spec child_spec(sup_id(_), handler(), config(_)) ->
    supervisor:child_spec().
child_spec(SupID, Handler, Config) ->
    MFA = {?MODULE, start_link, [Handler, Config]},
    #{
        id    => get_name(sup, Handler),
        start => {order_book_server_sup, start_link, [get_ref(sup, {Handler, SupID}), MFA]},
        type  => supervisor
    }.

-spec start(id(_), sup_id(_), init_args(_), handler()) ->
    {ok, pid()} | {error, {present, pid()}}.

start(ID, SupID, InitArgs, Handler) ->
    case supervisor:start_child(get_ref(sup, {Handler, SupID}), [ID, InitArgs]) of
        Ok = {ok, _Pid}                 -> Ok;
        {error, {already_started, Pid}} -> {error, {present, Pid}}
    end.

-spec start_link(handler(), config(_), id(_), init_args(_)) ->
    {ok, pid()} | {error, {already_started, pid()}}.
start_link(Handler, Config, ID, InitArgs) ->
    gen_server:start_link(get_ref(server, ID), ?MODULE, {Handler, ID, InitArgs, Config}, []).

-spec call(id(_), req(_)) ->
    _Reply | {error, notfound} | {error, notsupported}.
call(ID, Req) ->
    do_call(ID, fun(Ref) -> gen_server:call(Ref, Req) end).

-spec call(id(_), req(_), timeout()) ->
    _Reply | {error, notfound} | {error, notsupported}.
call(ID, Req, Timeout) ->
    do_call(ID, fun(Ref) -> gen_server:call(Ref, Req, Timeout) end).


-spec cast(id(_), req(_)) ->
    ok.
cast(ID, Req) ->
    gen_server:cast(get_ref(server, ID), Req).

-spec reply(caller(), _Reply) ->
    ok.
reply(To, Reply) ->
    _ = gen_server:reply(To, Reply),
    ok.

%% API utils

do_call(ID, Call) ->
    try Call(get_ref(server, ID))
    catch
        exit:noproc ->
            {error, notfound};
        exit:{noproc, {gen_server, call, _}} ->
            {error, notfound}
    end.

-spec get_ref(sup | server, id(_)) ->
    order_book_reg:ref().
get_ref(Type, ID) ->
    order_book_reg:get_ref(get_name(Type, ID)).

get_name(Type, ID) ->
    {?MODULE, {Type, ID}}.

%% gen_server behaviour

-type st() :: #{
    id         := id(_),
    handler    := handler(),
    handler_st := handler_st(_)
}.

-spec init({handler(), id(_), init_args(_), config(_)}) ->
    {ok, st(), {continue, {init, init_args(_)}}}.

init({H, ID, InitArgs, Config}) ->
    HSt = H:init_start(ID, Config),
    {ok, new_st(ID, H, HSt), {continue, {init, InitArgs}}}.


-spec handle_continue({init, init_args(_)}, st()) ->
    {noreply, st()} | {noreply, st(), timeout()}.

handle_continue({init, InitArgs}, St = #{handler := H, handler_st := HSt}) ->
    HSt1 = H:init_finish(InitArgs, HSt),
    {noreply, St#{handler_st => HSt1}}.

-spec handle_call(_Call, caller(), st()) ->
    {noreply, st()} | {reply, {error, notsupported}, st()}.
handle_call(Call, From, St) ->
    handle(call, Call, From, St).

-spec handle_cast(_Cast, st()) ->
    {noreply, st()}.
handle_cast(Cast, St) ->
    handle(cast, Cast, undefined, St).

-spec handle_info(_Info, st()) ->
    {noreply, st()}.
handle_info(Info, St) ->
    handle(info, Info, undefined, St).

-spec terminate(_Reason, st()) ->
    ok.
terminate(_Reason, _St) ->
    ok.

-spec code_change(_OldVsn, st(), _Extra) ->
    {ok, st()}.
code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% gen_server utils

new_st(ID, H, HSt) ->
    #{
        id         => ID,
        handler    => H,
        handler_st => HSt
    }.

-spec handle(req_type(), req(_), caller() | undefined, st()) ->
    {noreply, st()} | {reply, {error, notsupported}}.
handle(Type, Req = {Action, _}, From, St = #{handler := H, handler_st := HSt}) ->
    case H:is_supported(Type, Action) of
        true ->
            maybe_reply(Type, handle_supported(Type, From, H, Req, HSt), St);
        false ->
            handle_unsupported(Type, Req, From, St)
    end;
handle(Type, Req, From, St) ->
    handle_unsupported(Type, Req, From, St).

maybe_reply(call, {reply, Reply, HSt1}, St) ->
    {reply, Reply, St#{handler_st => HSt1}};
maybe_reply(call, HSt1, St) ->
    {reply, ok, St#{handler_st => HSt1}};
maybe_reply(_, HSt1, St) ->
    {noreply, St#{handler_st => HSt1}}.

handle_supported(call, From, H, Req, Hst) ->
    H:handle_call(Req, Hst, From);
handle_supported(cast, _, H, Req, Hst) ->
    H:handle_cast(Req, Hst);
handle_supported(info, _, H, Req, Hst) ->
    H:handle_info(Req, Hst).

handle_unsupported(call, Call, From, St = #{id := ID}) ->
    ok = logger:info("[~p]: unsupported call: ~p from ~p", [ID, Call, From]),
    {reply, {error, notsupported}, St};
handle_unsupported(cast, Cast, _, St = #{id := ID}) ->
    ok = logger:info("[~p]: unexpected cast: ~p", [ID, Cast]),
    {noreply, St};
handle_unsupported(info, Info, _, St = #{id := ID}) ->
    ok = logger:info("[~p]: unexpected info: ~p", [ID, Info]),
    {noreply, St}.
