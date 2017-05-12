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
-export([admin_get_frame/2]).


-export([nkadmin_session_init/2, nkadmin_session_terminate/2, nkadmin_session_event/3,
         nkadmin_session_reg_event/4, nkadmin_session_stop/2,
         nkadmin_session_handle_call/3, nkadmin_session_handle_cast/2,
         nkadmin_session_handle_info/2]).
-export([api_server_cmd/2, api_server_syntax/4]).
-export([api_server_reg_down/3]).

-include_lib("nkservice/include/nkservice.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").

-define(LLOG(Type, Txt, Args), "NkADMIN " ++ Txt, Args).


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
%% Admin Callbacks
%% ===================================================================

-type admin_id() :: nkadmin_session:id().
-type admin() :: nkadmin_session:admin().


admin_get_frame(SrvId, Data) ->
    case frame_domain(SrvId, Data) of
        {ok, Frame1} ->
            case frame_user(SrvId, Data) of
                {ok, Frame2} ->
                    case frame_user_menu(SrvId, Data) of
                        {ok, Frame3} ->
                            {ok, Frame1 ++ Frame2 ++ Frame3};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.





%% ===================================================================
%% Util
%% ===================================================================


%% @private
frame_domain(SrvId, #{domain_id:=DomainId}) ->
    case nkdomain:get_name(SrvId, DomainId) of
        {ok, #{name:=DomName, icon_id:=DomIconId}} ->
            {ok, [
                #{
                    id => 'admin.domain.name',
                    class => frameDomainName,
                    value => DomName
                },
                #{
                    id => 'admin.domain.icon',
                    class => frameDomainIcon,
                    value => DomIconId
                }
            ]};
        {error, Error} ->
            {error, Error}
    end;

frame_domain(_SrvId, _) ->
    {ok, []}.


%% @private
frame_user(SrvId, #{user_id:=UserId}) ->
    case nkdomain_user_obj:get_name(SrvId, UserId) of
        {ok, #{<<"user">>:=#{name:=UserName, surname:=UserSurname, icon_id:=UserIconId}}} ->
            {ok, [
                #{
                    id => 'admin.user.name',
                    class => userName,
                    value => <<UserName/binary, " ", UserSurname/binary>>
                },
                #{
                    id => 'admin.user.icon',
                    class => frameUserIcon,
                    value => UserIconId
                }
            ]};
        {error, Error} ->
            {error, Error}
    end;

frame_user(_SrvId, _) ->
    {ok, []}.


%% @private
frame_user_menu(_SrvId, #{user_menu:=true}) ->
    {ok, [
        #{
            id => 'admin.user.menu',
            class => frameUserMenu,
            value => #{
                items => [
                    #{
                        id => 'admin.user.menu.account',
                        class => frameUserMenuItem,
                        value => <<"My Account">>
                    },
                    #{
                        id => 'admin.user.menu.account',
                        class => frameUserMenuItem,
                        value => <<"My Account">>
                    },
                    #{
                        class => frameUserMenuSeparator
                    }
                ]
            }
        }
    ]};

frame_user_menu(_SrvId, _) ->
    {ok, []}.











%% @doc Called when a new call starts
-spec nkadmin_session_init(admin_id(), admin()) ->
    {ok, admin()}.

nkadmin_session_init(_Id, Admin) ->
    {ok, Admin}.

%% @doc Called when the call stops
-spec nkadmin_session_stop(Reason::term(), admin()) ->
    {ok, admin()}.

nkadmin_session_stop(_Reason, Admin) ->
    {ok, Admin}.


%% @doc Called when the call is destroyed
-spec nkadmin_session_terminate(Reason::term(), admin()) ->
    {ok, admin()}.

nkadmin_session_terminate(_Reason, Admin) ->
    {ok, Admin}.


%% @doc Called when the status of the call changes
-spec nkadmin_session_event(admin_id(), nkadmin_session:event(), admin()) ->
    {ok, admin()} | continue().

nkadmin_session_event(AdminId, Event, Admin) ->
    nkadmin_session_events:event(AdminId, Event, Admin).


%% @doc Called when the status of the call changes, for each registered
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
    #nkapi_req{class=nkadmin, subclass=Sub, cmd=Cmd}=Req, State) ->
    nkadmin_session_api:cmd(Sub, Cmd, Req, State);

api_server_cmd(_Req, _State) ->
    continue.


%% @private
api_server_syntax(#nkapi_req{class=nkadmin, subclass=Sub, cmd=Cmd},
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