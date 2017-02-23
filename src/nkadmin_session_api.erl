%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(nkadmin_session_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/4, api_down/3, session_stopped/3]).

-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Commands
%% ===================================================================


%% @doc An call start has been received
%% We create the call linked with the API server process
%% - we capture the destroy event %%
%%   (nkcollab_call_reg_event() -> call_hangup () here)
%% - if the session is killed, it is detected
%%   (api_server_reg_down() -> api_call_down() here)
cmd(session, create, Req, State) ->
    #api_req{srv_id=SrvId, data=Data, user_id=User, session_id=SessId} = Req,
    #{domain:=Domain} = Data,
    Config = Data#{
        srv_id => SrvId,
        domain => Domain,
        user_id => User,
        user_session_id => SessId,
        register => {nkadmin_api, self()}
    },
    case nkadmin_session:start(Config) of
        {ok, AdminId, Reply, Pid} ->
            nkservice_api_server:register(self(), {nkadmin_session, AdminId, Pid}),
            {ok, Reply#{admin_session_id=>AdminId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(session, switch_domain, #api_req{data=Data}, State) ->
    #{admin_session_id:=AdminId, domain:=Domain} = Data,
    case nkadmin_session:switch_domain(AdminId, Domain) of
        {ok, Reply} ->
            {ok, Reply, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(session, switch_object, #api_req{data=Data}, State) ->
    #{admin_session_id:=AdminId, obj_id:=ObjId} = Data,
    case nkadmin_session:switch_object(AdminId, ObjId) of
        {ok, Replay} ->
            {ok, Replay, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(session, destroy, #api_req{data=Data}, State) ->
    #{admin_session_id:=AdminId} = Data,
    case nkadmin_session:stop(AdminId, api_stop) of
        ok ->
            {ok, #{}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(_Sub, _Cmd, _Req, _State) ->
    continue.


%% ===================================================================
%% Public functions
%% ===================================================================

%% @private Sent by the admin session when it has been stopped
%% We sent a message to the API session to remove the admin session before
%% it receives the DOWN.
session_stopped(AdminId, ApiPid, _Admin) ->
    nkservice_api_server:unregister(ApiPid, {nkadmin_session, AdminId, self()}),
    unsubscribe(AdminId, ApiPid).



%% @private Called when API server detects a registered admin session is down
%% Normally it should have been unregistered first
%% (detected above and sent in the cast after)

api_down(AdminId, Reason, State) ->
    #{srv_id:=SrvId} = State,
    lager:warning("Admin Session ~s is down: ~p", [AdminId, Reason]),
    nkadmin_session_events:event_session_down(SrvId, AdminId, self()),
    unsubscribe(AdminId, self()).



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
unsubscribe(AdminId, ConnId) ->
    Fun = fun(#event{class=Class, obj_id=ObjId}) ->
        nklib_util:to_binary(Class) == <<"admin">> andalso ObjId == AdminId
    end,
    nkservice_api_server:unsubscribe_fun(ConnId, Fun).
