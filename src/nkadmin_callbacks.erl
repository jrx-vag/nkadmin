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
-export([admin_get_frame/1, admin_get_tree/1, admin_get_detail/1, admin_event/3]).
-export([admin_get_menu_categories/2, admin_menu_fill_category/3]).
-export([api_server_cmd/2, api_server_syntax/4]).
-export([api_server_reg_down/3]).

-include_lib("nkapi/include/nkapi.hrl").
-include_lib("nkevent/include/nkevent.hrl").

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

-type state() :: #{
    srv_id => nkservice:id(),
    domain_id => nkdomain:obj_id(),
    user_id => nkdomain:obj_id(),
    language => binary(),
    types => #{nkdomain:type() => integer()}
}.

%%-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Admin Callbacks
%% ===================================================================

%% @doc Must return the frame elements
-spec admin_get_frame(state()) ->
    {ok, map(), state()} | {error, term(), state()}.

admin_get_frame(State) ->
    nkadmin_frame:get_frame(State).


%% @doc Must return the tree elements
-spec admin_get_tree(state()) ->
    {ok, map(), state()} | {error, term(), state()}.

admin_get_tree(State) ->
    nkadmin_tree:get_tree(State).


%% @doc Must return the detail elements
-spec admin_get_detail(state()) ->
    {ok, map(), state()} | {error, term(), state()}.

admin_get_detail(State) ->
    nkadmin_detail:get_detail(State).


%% @doc Called when a registered event is received
%% Must reply with updates for the client
-spec admin_event(#nkevent{}, list(), state()) ->
    {ok, list(), state()} | {error, term(), state()}.

admin_event(Event, UpdBase, State) ->
    {ok, Upd1, State2} = nkadmin_frame:event(Event, State),
    {ok, Upd2, State3} = nkadmin_tree:event(Event, State2),
    {ok, Upd3, State4} = nkadmin_detail:event(Event, State3),
    {ok, UpdBase++Upd1++Upd2++Upd3, State4}.


%% @doc Must add desired categories as a map with the position (lower first)
-spec admin_get_menu_categories(map(), state()) ->
    {ok, map(), state()}.

admin_get_menu_categories(Map, State) ->
    Data = #{
        overview => 1000,
        resources => 1100,
        sessions => 1200,
        networks => 1300,
        services => 1400
    },
    {ok, maps:merge(Data, Map), State}.


%% @doc
admin_menu_fill_category(overview, Acc, State) ->
    AdminData = #{
        id => menu_overview,
        class => menuCategory,
        value => #{label=>i18n(menu_overview, State)},
        entries => [
            #{
                id => menu_overview_dashboard,
                class => menuSimple,
                value => #{label=> i18n(menu_overview_dashboard, State)}
            }
        ]
    },
    {ok, nklib_util:map_merge(Acc, AdminData), State};

admin_menu_fill_category(Category, Acc, State)
        when Category==resources; Category==sessions; Category==networks; Category==services ->
    case map_size(Acc) of
        0 ->
            {ok, #{}, State};
        _ ->
            Id = case Category of
                resources -> menu_resources;
                sessions -> menu_sessions;
                networks -> menu_networks;
                services -> menu_services
            end,
            AdminData = #{
                id => Id,
                class => menuCategory,
                value => #{value => i18n(Id, State)}
            },
            {ok, maps:merge(AdminData, Acc), State}
    end;

admin_menu_fill_category(_, Acc, State) ->
    {ok, Acc, State}.


%% ===================================================================
%% Util
%% ===================================================================

%% @private
i18n(Key, Data) ->
    nkadmin_util:i18n(Key, Data).


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