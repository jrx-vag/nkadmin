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
-export([admin_get_frame/1, admin_get_tree/1, admin_get_detail/1]).
-export([admin_get_menu_categories/2, admin_menu_fill_category/3]).
-export([api_server_cmd/2, api_server_syntax/4]).
-export([api_server_reg_down/3]).

%%-include_lib("nkservice/include/nkservice.hrl").
%%-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkapi/include/nkapi.hrl").
%%-include("nkadmin.hrl").

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
%%-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Admin Callbacks
%% ===================================================================

%% @doc
admin_get_frame(State) ->
    nkadmin_frame:get_frame(State).


%% @doc
admin_get_tree(State) ->
    nkadmin_tree:get_tree(State).


%% @doc
admin_get_detail(State) ->
    nkadmin_detail:get_detail(State).



%% @doc
admin_get_menu_categories(_SrvId, Map) ->
    Data = #{
        overview => 1000,
        resources => 1100,
        sessions => 1200,
        networks => 1300,
        services => 1400
    },
    maps:merge(Data, Map).


%% @doc
admin_menu_fill_category(overview, Data, Acc) ->
    AdminData = #{
        id => menu_overview,
        class => menuCategory,
        value => i18n(menu_overview, Data),
        entries => [
            #{
                id => menu_overview_dashboard,
                class => menuSimple,
                value =>  i18n(menu_overview_dashboard, Data)
            }
        ]
    },
    nklib_util:map_merge(Acc, AdminData);

admin_menu_fill_category(Category, Data, Acc)
        when Category==resources; Category==sessions; Category==networks; Category==services ->
    case map_size(Acc) of
        0 ->
            #{};
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
                value => i18n(Id, Data)
            },
            maps:merge(AdminData, Acc)
    end;

admin_menu_fill_category(_, _Data, Acc) ->
    Acc.


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