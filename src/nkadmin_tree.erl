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
    load_categories(lists:reverse(Categories), [], State2).


%% @doc
event(#nkevent{}, State) ->
    {ok, [], State}.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
get_categories(#{srv_id:=SrvId}=State) ->
    {ok, Categories1, State2} = SrvId:admin_tree_categories(#{}, State),
    Categories2 = [{Weight, Key} || {Key, Weight} <- maps:to_list(Categories1)],
    {ok, [Key || {_Weight, Key} <- lists:sort(Categories2)], State2}.


%% @private
load_categories([], Acc, State) ->
    io:format("NKLOG ~s\n", [nklib_json:encode_pretty(Acc)]),
    {ok, Acc, State};

load_categories([Category|Rest], Acc, #{srv_id:=SrvId}=State) ->
    case SrvId:admin_tree_get_category(Category, State) of
        {ok, Map, State2} when map_size(Map)==0 ->
            load_categories(Rest, Acc, State2);
        {ok, Map, State2} when is_map(Map) ->
            load_categories(Rest, [Map|Acc], State2)
    end.



