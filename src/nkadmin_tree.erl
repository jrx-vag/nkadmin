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

-module(nkadmin_tree).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([get_tree/0, get_tree/1, event/2]).
-export([add_tree_entry/4]).

-include_lib("nkevent/include/nkevent.hrl").



%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Public functions
%% ===================================================================

get_tree() ->
    get_tree(#{srv_id=>root, domain_id=>root, language=>es}).


%% @doc
get_tree(State) ->
    {ok, Categories, State2} = get_categories(State),
    Now = nklib_util:l_timestamp(),
    R = load_categories(lists:reverse(Categories), [], State2),
    Time = nklib_util:l_timestamp() - Now,
    lager:error("NKLOG Time ~p", [Time / 1000]),
    R.


%% @doc
event(#nkevent{}, State) ->
    {ok, [], State}.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
get_categories(#{srv_id:=SrvId}=State) ->
    {ok, Categories1, State2} = SrvId:admin_get_menu_categories(#{}, State),
    Categories2 = [{Weight, Key} || {Key, Weight} <- maps:to_list(Categories1)],
    {ok, [Key || {_Weight, Key} <- lists:sort(Categories2)], State}.


%% @private
load_categories([], Acc, State) ->
    io:format("NKLOG ~s\n", [nklib_json:encode_pretty(Acc)]),
    {ok, Acc, State};

load_categories([Category|Rest], Acc, #{srv_id:=SrvId}=State) ->
    case SrvId:admin_menu_fill_category(Category, #{}, State) of
        {ok, Map, State2} when map_size(Map)==0 ->
            load_categories(Rest, Acc, State2);
        {ok, Map, State2} ->
            load_categories(Rest, [Map|Acc], State2)
    end.



add_tree_entry(Id, Class, Data, Acc) ->
    Entries = [
        case Class of
            menuSimple ->
                #{
                    id => Id,
                    class => menuSimple,
                    value => #{label=>i18n(Id, Data)}
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
