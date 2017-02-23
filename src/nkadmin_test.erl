-module(nkadmin_test).
-author("carlosj").

%% API

-define(WS, "ws://127.0.0.1:10002/admin").
-define(HTTP, "http://127.0.0.1:100002/admin").

-compile([export_all]).




%% @doc
start() ->
    start("ws:all:10002/admin, http://all:10002/admin").


%% @doc
start(Path) ->
    Spec = #{
        callback => ?MODULE,
        plugins => [nkadmin],
        api_server => Path,
        api_server_timeout => 180,
        debug => [nkservice_api_server, nkadmin_session]
    },
    nkservice:start(admin, Spec).


%% @doc
stop() ->
    nkservice:stop(admin).



%% @doc
login() ->
    Fun = fun ?MODULE:api_client_fun/2,
    Login = #{user => <<"test">>, password => <<>>},
    {ok, _, C, _} = nkservice_api_client:start(admin, ?WS, Login, Fun, #{}),
    C.


create() ->
    Data = #{
        domain => <<"/">>,
        session_events => [created, destroyed]
    },
    {ok, #{<<"admin_session_id">>:=SessId}} = cmd(create, Data),
    SessId.


switch_domain(A, D) ->
    cmd(switch_domain, #{admin_session_id=>A, domain=>D}).

switch_object(A, O) ->
    cmd(switch_object, #{admin_session_id=>A, obj_id=>O}).


destroy(A) ->
    cmd(destroy, #{admin_session_id=>A}).




%% ===================================================================
%% Service callbacks
%% ===================================================================

api_server_allow(_Req, State) ->
%%    lager:error("Allow: ~p", [_Req]),
    {true, State}.


api_server_login(#{user:=User}, State) ->
    {true, User, #{}, State}.



%% ===================================================================
%% Internal
%% ===================================================================


api_client_fun(_Req, UserData) ->
    lager:error("TEST CLIENT req: ~p", [lager:pr(_Req, ?MODULE)]),
    {error, not_implemented, UserData}.


get_client() ->
    [{_, Pid}|_] = nkservice_api_client:get_all(),
    Pid.


cmd(Cmd, Data) ->
    cmd(get_client(), Cmd, Data).


cmd(Pid, Cmd, Data) ->
    nkservice_api_client:cmd(Pid, admin, session, Cmd, Data).


cmd_http(Sub, Cmd, Data) ->
    Body = #{
        class => admin,
        subclass => Sub,
        cmd => Cmd,
        data => Data
    },
    nkservice_util:http(post, [?HTTP, "/rpc"], #{body=>Body}).

