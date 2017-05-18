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
-export([admin_tree_categories/2, admin_tree_get_category/2]).

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
-spec admin_tree_categories(map(), state()) ->
    {ok, map(), state()}.

admin_tree_categories(Map, State) ->
    {ok, Map, State}.


%% @doc
admin_tree_get_category(_Category, State) ->
    {ok, #{}, State}.


