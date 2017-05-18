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

-module(nkadmin_util).
-export([i18n/2, menu_item/3]).



%% ===================================================================
%% Public
%% ===================================================================


%% @private
i18n(Key, Data) ->
    nklib_i18n:get(Key, maps:get(language, Data, <<"en">>)).


%% @doc
menu_item(Id, {menuCategory, Entries}, State) ->
    #{
        id => Id,
        class => menuCategory,
        value => #{
            label => nkadmin_util:i18n(Id, State),
            entries => Entries
        }
    };

menu_item(Id, menuSimple, State) ->
    #{
        id => Id,
        class => menuSimple,
        value => #{label => nkadmin_util:i18n(Id, State)}
    };

menu_item(Id, {menuBadge, Num}, State) ->
    #{
        id => Id,
        class => menuBadge,
        value => #{
            label => nkadmin_util:i18n(Id, State),
            badge => Num
        }
    }.
