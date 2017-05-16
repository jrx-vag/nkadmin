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

-module(nkadmin_i18n).
-behavior(nklib_i18n).

-export([i18n/0, reload/0]).


i18n() -> #{
    en => #{
        frame_user_menu_account => "My account",
        frame_user_menu_messages => "My messages",

        menu_overview => "Overview",
        menu_overview_dashboard => "Dashboard",

        menu_resources => "Resources",
        menu_resources_users => "Users",
        menu_resources_configs => "Configurations",
        menu_resources_chat_messages => "Chat Messages",
        menu_resources_chat_conversations => "Conversations",

        menu_sessions => "Sessions",
        menu_sessions_login => <<"Login">>,
        menu_sessions_tokens => <<"Tokens">>,
        menu_sessions_chat_sessions => <<"Chat">>,
        menu_sessions_admin => "Admin",

        menu_networks => "Networs",
        menu_services => "Services"
    },
    es => #{
        frame_user_menu_account => "Mi cuenta",
        frame_user_menu_messages => "Mis mensajes",

        menu_overview => "General",
        menu_overview_dashboard => <<"Información"/utf8>>,

        menu_resources => "Recursos",
        menu_resources_users => "Usuarios",
        menu_resources_configs => "Configuraciones",
        menu_resources_chat_messages => "Mensajes de chat",
        menu_resources_chat_conversations => "Conversaciones",

        menu_sessions => "Sesiones",
        menu_sessions_login => <<"Login">>,
        menu_sessions_tokens => <<"Tokens">>,
        menu_sessions_chat_sessions => <<"Chat">>,
        menu_sessions_admin => "Admin",

        menu_networks => "Redes",
        menu_services => "Servicios"

    }
}.


reload() ->
    ok = nklib_i18n:load(?MODULE).