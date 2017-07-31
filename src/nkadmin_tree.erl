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

-export([get_tree/1]).

-include("nkadmin.hrl").
-include_lib("nkevent/include/nkevent.hrl").



%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% Public functions
%% ===================================================================

%% @doc
get_tree(Session) ->
    case get_categories(Session) of
        {ok, Categories, Session2} ->
            case load_categories(lists:reverse(Categories), [], Session2) of
                {ok, List, Session3} ->
                    {ok, #{class=>tree, value=>#{items=>List}}, Session3};
                {error, Error, Session2} ->
                    {error, Error, Session2}
            end;
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
get_categories(#admin_session{srv_id=SrvId}=Session) ->
    case SrvId:admin_tree_categories(#{}, Session) of
        {ok, Categories1, Session2} ->
            Categories2 = [{Weight, Key} || {Key, Weight} <- maps:to_list(Categories1)],
            {ok, [Key || {_Weight, Key} <- lists:sort(Categories2)], Session2};
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.


%% @private
load_categories([], Acc, Session) ->
    {ok, Acc, Session};

load_categories([Category|Rest], Acc, #admin_session{srv_id=SrvId}=Session) ->
    case SrvId:admin_tree_get_category(Category, Session) of
        {ok, Map, Session2} when map_size(Map)==0 ->
            load_categories(Rest, Acc, Session2);
        {ok, Map, Session2} when is_map(Map) ->
            load_categories(Rest, [Map|Acc], Session2);
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.



