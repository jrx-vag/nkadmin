-module(nkadmin_sample).
-author("carlosj").

-include("nkadmin.hrl").

%% API

-compile([export_all]).


%% @doc
login() ->
    nkdomain_sample:login().


session_find() ->
    cmd(<<"objects/admin.session/find">>, #{}).

session_find(UserId) ->
    cmd(<<"objects/admin.session/find">>, #{user_id=>UserId}).

session_create() ->
    cmd(<<"objects/admin.session/create">>, #{}).

session_create(UserId) ->
    cmd(<<"objects/admin.session/create">>, #{user_id=>UserId}).

session_start() ->
    cmd(<<"objects/admin.session/start">>, #{language=>en, url=>"/url"}).

session_start(SessId) ->
    cmd(<<"objects/admin.session/start">>, #{id=>SessId}).

session_get() ->
    cmd(<<"objects/admin.session/get">>, #{}).

session_get(SessId) ->
    cmd(<<"objects/admin.session/get">>, #{id=>SessId}).

session_stop() ->
    cmd(<<"objects/admin.session/stop">>, #{}).

session_switch_domain(Domain) ->
    {ok, D} = cmd(<<"objects/admin.session/switch_domain">>, #{domain_id=>Domain}),
    Str = nklib_json:encode_pretty(D),
    io:format("~s\n", [Str]),
    ok.

session_action(ElementId) ->
    cmd(<<"objects/admin.session/element_action">>, #{element_id=>ElementId, action=>selected}).


%% ===================================================================
%% Client fun
%% ===================================================================



%% Test calling with class=test, cmd=op1, op2, data=#{nim=>1}
cmd(Cmd, Data) ->
    Pid = nkdomain_sample:get_client(),
    cmd(Pid, Cmd, Data).

cmd(Pid, Cmd, Data) ->
    nkapi_client:cmd(Pid, Cmd, Data).

