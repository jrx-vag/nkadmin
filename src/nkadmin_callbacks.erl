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

-export([plugin_deps/0, plugin_syntax/0, plugin_listen/2]).
-export([error/1]).
-export([admin_get_frame/1, admin_get_tree/1]).
-export([admin_event/3, admin_element_action/5, admin_get_data/3]).
-export([admin_tree_categories/2, admin_tree_get_category/2]).

-include_lib("nkevent/include/nkevent.hrl").

-define(LLOG(Type, Txt, Args), "NkADMIN " ++ Txt, Args).


%% ===================================================================
%% Config callbacks
%% ===================================================================


plugin_deps() ->
    [
    ].


plugin_syntax() ->
    nkpacket_util:get_plugin_net_syntax(#{
        admin_url => fun nkservice_webserver_util:parse_web_server/1
    }).


plugin_listen(Config, #{id:=SrvId}) ->
    {parsed_urls, WebSrv} = maps:get(admin_url, Config, {parsed_urls, []}),
    Priv = list_to_binary(code:priv_dir(nkadmin)),
    Path = <<Priv/binary, "/www">>,
    nkservice_webserver_util:get_web_servers(SrvId, WebSrv, Path, Config).




%% ===================================================================
%% Types
%% ===================================================================


%%-type continue() :: continue | {continue, list()}.
-type session() :: nkadmin_session_obj:session().


%% ===================================================================
%% Errors
%% ===================================================================

%% @doc
error(unrecognized_element)      -> "Element not recognized";
error(_)   		                 -> continue.



%% ===================================================================
%% Admin Callbacks
%% ===================================================================

%% @doc Must return the frame elements
-spec admin_get_frame(session()) ->
    {ok, map(), session()} | {error, term(), session()}.

admin_get_frame(Session) ->
    nkadmin_frame:get_frame(Session).


%% @doc Must return the tree elements
-spec admin_get_tree(session()) ->
    {ok, map(), session()} | {error, term(), session()}.

admin_get_tree(Session) ->
    nkadmin_tree:get_tree(Session).



%% @doc Called when a registered event is received
%% Must reply with updates for the client
%% Domain and other will add reply before reaching here
-spec admin_event(#nkevent{}, list(), session()) ->
    {ok, list(), session()} | {error, term(), session()}.

admin_event(Event, Updates, Session) ->
    nkadmin_frame:event(Event, Updates, Session).


%% @doc Called when an action on an element has been received
-spec admin_element_action(binary(), binary(), term(), list(), session()) ->
    {ok, list(), session()} | {error, term(), session()}.

admin_element_action(ElementId, Action, Value, Updates, Session) ->
    nkadmin_frame:element_action(ElementId, Action, Value, Updates, Session).


%% @doc Called when the client asks for specific table data
-spec admin_get_data(binary(), map(), session()) ->
    {ok, map(), session()} | {error, term(), session()}.

admin_get_data(_ElementId, _Spec, Session) ->
    {error, unrecognized_element, Session}.


%% @doc Called from nkadmin_tree to get the list of categories
%% Must return  a map with the position (lower first)
-spec admin_tree_categories(map(), session()) ->
    {ok, map(), session()}.

admin_tree_categories(Map, Session) ->
    {ok, Map, Session}.


%% @doc Called from nkadmin_tree to get the data of a category
%% Must return  a map with the position (lower first)
-spec admin_tree_get_category(binary(), session()) ->
    {ok, map(), session()} | {error, term()}.

admin_tree_get_category(_Category, Session) ->
    {ok, #{}, Session}.


