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

-module(nkadmin_callbacks).

-export([plugin_deps/0]).
-export([api_server_cmd/2, api_server_syntax/4]).
-export([api_server_reg_down/3]).
-include_lib("nkservice/include/nkservice.hrl").



%% ===================================================================
%% Config callbacks
%% ===================================================================


plugin_deps() ->
    [
    ].





%% ===================================================================
%% Types
%% ===================================================================

% -type state() :: term().
% -type continue() :: continue | {continue, list()}.



%% ===================================================================
%% MM proxy
%% ===================================================================

%% ===================================================================
%% API CMD
%% ===================================================================

%% @private
api_server_cmd(
    #api_req{class=admin, subclass=Sub, cmd=Cmd}=Req, State) ->
    nkadmin_api:cmd(Sub, Cmd, Req, State);

api_server_cmd(_Req, _State) ->
    continue.


%% @privat
api_server_syntax(#api_req{class=admin, subclass=Sub, cmd=Cmd},
                  Syntax, Defaults, Mandatory) ->
    nkadmin_syntax:syntax(Sub, Cmd, Syntax, Defaults, Mandatory);

api_server_syntax(_Req, _Syntax, _Defaults, _Mandatory) ->
    continue.


%% ===================================================================
%% API Server
%% ===================================================================

%% @private
api_server_reg_down({nkadmin_session, AdminId, _Pid}, Reason, State) ->
    nkadmin_api:api_down(AdminId, Reason, State),
    continue;

api_server_reg_down(_Link, _Reason, _State) ->
    continue.