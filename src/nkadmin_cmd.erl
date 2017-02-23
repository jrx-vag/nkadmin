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

-module(nkadmin_cmd).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/4]).

-include_lib("nkservice/include/nkservice.hrl").


%% ===================================================================
%% Types
%% ===================================================================




%% ===================================================================
%% Public functions
%% ===================================================================


%% @doc An call start has been received
%% We create the call linked with the API server process
%% - we capture the destroy event %%
%%   (nkcollab_call_reg_event() -> call_hangup () here)
%% - if the session is killed, it is detected
%%   (api_server_reg_down() -> api_call_down() here)
cmd('', create, Req, State) ->
    #api_req{srv_id=SrvId, data=Data, user_id=User, session_id=SessId} = Req,
    #{domain:=Domain} = Data,
    Config = Data#{
        srv_id = SrvId,
        domain = Domain,
        user_id => User,
        user_session_id => SessId,
        uses_session_pid => self()
    },
    case nkadmin_session:start(Config) of
        {ok, AdminSessId, Data, Pid} ->
            nkservice_api_server:register(self(), {nkadmin_api, AdminSessId, Pid}),
            {ok, Data#{session_id=>AdminSessId}, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', switch_domain, #api_req{data=Data}, State) ->
    #{admin_session_id:=AdminSessId, domain:=Domain} = Data,
    case nkadmin_session:switch_domain(AdminSessId, Domain) of
        {ok, Data} ->
            {ok, Data, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd('', switch_object, #api_req{data=Data}, State) ->
    #{admin_session_id:=AdminSessId, obj_id:=ObjId} = Data,
    case nkadmin_session:switch_object(AdminSessId, ObjId) of
        {ok, Data} ->
            {ok, Data, State};
        {error, Error} ->
            {error, Error, State}
    end;

cmd(_Sub, _Cmd, _Req, _State) ->
    continue.
