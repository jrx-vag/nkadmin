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

-module(nkadmin_test_data).
-export([frame/2, tree/2, detail/2]).


%% @doc Called to get the current frame
-spec frame(UserId::binary(), Domain::binary()) ->
    {ok, nkadmin_session:frame()} | {error, term()}.

frame(UserId, Domain) ->
    {ok, #{type=>frame, user=>UserId, domain=>Domain}}.


%% @doc Called to get the current tree
-spec tree(UserId::binary(), Domain::binary()) ->
    {ok, nkadmin_session:tree()} | {error, term()}.

tree(UserId, Domain) ->
    {ok ,#{type=>tree, user=>UserId, domain=>Domain}}.


%% @doc Called to get the current detail
-spec detail(UserId::binary(), ObjId::binary()) ->
    {ok, nkadmin_session:detail()} | {error, term()}.

detail(UserId, ObjId) ->
    {ok, #{type=>detail, user=>UserId, obj=>ObjId}}.
