-module(nkadmin_sample).
-author("carlosj").

-include("nkadmin.hrl").

%% API

-compile([export_all]).


%% @doc
login() ->
    nkdomain_sample:login().


session_find() ->
    cmd(?DOMAIN_ADMIN_SESSION, find, #{}).

session_find(UserId) ->
    cmd(?DOMAIN_ADMIN_SESSION, find, #{user_id=>UserId}).

session_create() ->
    cmd(?DOMAIN_ADMIN_SESSION, create, #{}).

session_create(UserId) ->
    cmd(?DOMAIN_ADMIN_SESSION, create, #{user_id=>UserId}).

session_start() ->
    cmd(?DOMAIN_ADMIN_SESSION, start, #{}).

session_start(SessId) ->
    cmd(?DOMAIN_ADMIN_SESSION, start, #{id=>SessId}).

session_get() ->
    cmd(?DOMAIN_ADMIN_SESSION, get, #{}).

session_get(SessId) ->
    cmd(?DOMAIN_ADMIN_SESSION, get, #{id=>SessId}).

session_stop() ->
    cmd(?DOMAIN_ADMIN_SESSION, stop, #{}).

session_switch_domain(Domain) ->
    cmd(?DOMAIN_ADMIN_SESSION, switch_domain, #{domain_id=>Domain}).

session_start_detail(ObjId) ->
    cmd(?DOMAIN_ADMIN_SESSION, start_detail, #{detail_id=>ObjId}).

session_stop_detail(ObjId) ->
    cmd(?DOMAIN_ADMIN_SESSION, stop_detail, #{detail_id=>ObjId}).


%% ===================================================================
%% Client fun
%% ===================================================================



%% Test calling with class=test, cmd=op1, op2, data=#{nim=>1}
cmd(Class, Cmd, Data) ->
    Pid = nkdomain_sample:get_client(),
    cmd(Pid, Class, Cmd, Data).

cmd(Pid, Class, Cmd, Data) ->
    nkapi_client:cmd(Pid, Class, <<>>, Cmd, Data).

