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
-export([nkadmin_session_init/2, nkadmin_session_terminate/2, nkadmin_session_event/3,
         nkadmin_session_reg_event/4, nkadmin_session_stop/2,
         nkadmin_session_handle_call/3, nkadmin_session_handle_cast/2,
         nkadmin_session_handle_info/2]).
-export([api_server_cmd/2, api_server_syntax/4]).
-export([api_server_reg_down/3]).

-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkapi/include/nkapi.hrl").



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
-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Admin Adminbacks
%% ===================================================================

-type admin_id() :: nkadmin_session:id().
-type admin() :: nkadmin_session:admin().


%% @doc Admined when a new call starts
-spec nkadmin_session_init(admin_id(), admin()) ->
    {ok, admin()}.

nkadmin_session_init(_Id, Admin) ->
    {ok, Admin}.

%% @doc Admined when the call stops
-spec nkadmin_session_stop(Reason::term(), admin()) ->
    {ok, admin()}.

nkadmin_session_stop(_Reason, Admin) ->
    {ok, Admin}.


%% @doc Admined when the call is destroyed
-spec nkadmin_session_terminate(Reason::term(), admin()) ->
    {ok, admin()}.

nkadmin_session_terminate(_Reason, Admin) ->
    {ok, Admin}.


%% @doc Admined when the status of the call changes
-spec nkadmin_session_event(admin_id(), nkadmin_session:event(), admin()) ->
    {ok, admin()} | continue().

nkadmin_session_event(AdminId, Event, Admin) ->
    nkadmin_session_events:event(AdminId, Event, Admin).


%% @doc Admined when the status of the call changes, for each registered
%% process to the session
-spec nkadmin_session_reg_event(admin_id(), nklib:link(), nkadmin_session:event(), admin()) ->
    {ok, admin()} | continue().

nkadmin_session_reg_event(AdminId, {nkadmin_api, ApiPid}, {stopped, _Reason}, Admin) ->
    nkadmin_session_api:session_stopped(AdminId, ApiPid, Admin),
    {ok, Admin};


nkadmin_session_reg_event(_AdminId, _Link, _Event, Admin) ->
    {ok, Admin}.


%% @doc
-spec nkadmin_session_handle_call(term(), {pid(), term()}, admin()) ->
    {reply, term(), admin()} | {noreply, admin()} | continue().

nkadmin_session_handle_call(Msg, _From, Admin) ->
    lager:error("Module nkadmin_session received unexpected call: ~p", [Msg]),
    {noreply, Admin}.


%% @doc
-spec nkadmin_session_handle_cast(term(), admin()) ->
    {noreply, admin()} | continue().

nkadmin_session_handle_cast(Msg, Admin) ->
    lager:error("Module nkadmin_session received unexpected call: ~p", [Msg]),
    {noreply, Admin}.


%% @doc
-spec nkadmin_session_handle_info(term(), admin()) ->
    {noreply, admin()} | continue().

nkadmin_session_handle_info(Msg, Admin) ->
    lager:warning("Module nkadmin_session received unexpected info: ~p", [Msg]),
    {noreply, Admin}.



%% ===================================================================
%% API CMD
%% ===================================================================

%% @private
api_server_cmd(
    #nkapi_req{class=admin, subclass=Sub, cmd=Cmd}=Req, State) ->
    nkadmin_session_api:cmd(Sub, Cmd, Req, State);

api_server_cmd(_Req, _State) ->
    continue.


%% @privat
api_server_syntax(#nkapi_req{class=admin, subclass=Sub, cmd=Cmd},
                  Syntax, Defaults, Mandatory) ->
    nkadmin_session_syntax:syntax(Sub, Cmd, Syntax, Defaults, Mandatory);

api_server_syntax(_Req, _Syntax, _Defaults, _Mandatory) ->
    continue.


%% ===================================================================
%% API Server
%% ===================================================================

%% @private
api_server_reg_down({nkadmin_session, AdminId, _Pid}, Reason, State) ->
    nkadmin_session_api:api_down(AdminId, Reason, State),
    continue;

api_server_reg_down(_Link, _Reason, _State) ->
    continue.