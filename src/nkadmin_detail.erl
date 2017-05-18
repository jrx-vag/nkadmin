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

-module(nkadmin_detail).
-export([get_detail/1, event/2]).

-include_lib("nkevent/include/nkevent.hrl").


%%-include_lib("nkservice/include/nkservice.hrl").
%%-include_lib("nkdomain/include/nkdomain.hrl").
%%-include_lib("nkapi/include/nkapi.hrl").
%%-include("nkadmin.hrl").
%%
-define(LLOG(Type, Txt, Args), "NkADMIN " ++ Txt, Args).



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
get_detail(State) ->
    {ok, #{}, State}.


%% @doc
event(#nkevent{}, State) ->
    {ok, [], State}.