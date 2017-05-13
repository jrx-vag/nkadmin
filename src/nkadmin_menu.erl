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

-module(nkadmin_menu).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-compile(export_all).
-export([add_tree_entry/4]).

%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Public functions
%% ===================================================================

get_menu() ->
    get_menu(#{srv_id=>root, domain_id=>root, language=>es}).

get_menu(#{srv_id:=SrvId, domain_id:=DomainId}=Data) ->
    Categories1 = SrvId:admin_get_menu_categories(SrvId, #{}),
    Categories2 = [{Weight, Key} || {Key, Weight} <- maps:to_list(Categories1)],
    Categories3 = [Key || {_Weight, Key} <- lists:sort(Categories2)],
    case nkdomain_domain_obj:find_all_types(SrvId, DomainId, #{}) of
        {ok, _, TypeList} ->
            Data2 = Data#{types=>maps:from_list(TypeList)},
            Now = nklib_util:l_timestamp(),
            R = load_categories(lists:reverse(Categories3), Data2, []),
            Time = nklib_util:l_timestamp() - Now,
            lager:error("NKLOG Time ~p", [Time / 1000]),
            R;

        {error, Error} ->
            {error, Error}
    end.


load_categories([], _Data, Acc) ->
    io:format("NKLOG ~s\n", [nklib_json:encode_pretty(Acc)]),
    {ok, Acc};

load_categories([Category|Rest], #{srv_id:=SrvId}=Data, Acc) ->
    Acc2 = case SrvId:admin_menu_fill_category(Category, Data, #{}) of
        Map when map_size(Map)==0 ->
            Acc;
        Map ->
            [Map|Acc]
    end,
    load_categories(Rest, Data, Acc2).


%% ===================================================================
%% Public functions
%% ===================================================================

add_tree_entry(Id, Class, Data, Acc) ->
    Entries = [
        case Class of
            menuSimple ->
                #{
                    id => Id,
                    class => menuSimple,
                    value => i18n(Id, Data)
                };
            {menuBadge, Num} ->
                #{
                    id => Id,
                    class => menuBadge,
                    value => #{
                        label => i18n(Id, Data),
                        badge => Num
                    }
                }
        end
        |
        maps:get(entries, Acc, [])
    ],
    Acc#{entries=>Entries}.


%% @private
i18n(Key, Data) ->
    nkadmin_util:i18n(Key, Data).
