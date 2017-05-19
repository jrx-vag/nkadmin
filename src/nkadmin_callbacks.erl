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
-export([api_error/1]).
-export([admin_get_frame/1, admin_get_tree/1, admin_get_detail/1, admin_event/3, admin_element_action/4]).
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
    types => #{nkdomain:type() => integer()},
    elements => #{binary() => term()}
}.

%%-type continue() :: continue | {continue, list()}.



%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
api_error(unrecognized_element)      -> "Element not recognized";
api_error(_)   		                 -> continue.



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
%% Domain and other will add reply before reaching here
-spec admin_event(#nkevent{}, list(), state()) ->
    {ok, list(), state()} | {error, term(), state()}.

admin_event(Event, Updates, State) ->
    nkadmin_frame:event(Event, Updates, State).


%% @doc Must add desired categories as a map with the position (lower first)
-spec admin_tree_categories(map(), state()) ->
    {ok, map(), state()}.

admin_tree_categories(Map, State) ->
    {ok, Map, State}.


%% @doc
admin_tree_get_category(_Category, State) ->
    {ok, #{}, State}.


%% @doc
admin_element_action(_ElementId, _Action, _Value, _State) ->
    {error, unrecognized_element}.
