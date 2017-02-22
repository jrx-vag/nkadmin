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

-module(nkadmin_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).
-export([get/1, get/2, get_srv/2, put/2]).

-define(APP, nkadmin).


%% doc Start manually
start() ->
    case nklib_util:ensure_all_started(?APP, permanent) of
        {ok, _Started} ->
            ok;
        Error ->
            Error
    end.


%% @private OTP standard start callback
start(_Type, _Args) ->
	Syntax = #{
    },
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Pid} = nkadmin_sup:start_link(),
            {ok, Vsn} = application:get_key(?APP, vsn),
            lager:info("NkADMIN v~s has started.", [Vsn]),
            {ok, Pid};
        {error, Error} ->
            lager:error("Config error: ~p", [Error]),
            error(config_error)
    end.



%% @private OTP standard stop callback
stop(_) ->
    ok.




%% Config Management
get(Key) ->
    nklib_config:get(?APP, Key).

get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).

get_srv(Class, Key) ->
    nklib_config:get_domain(?APP, Class, Key).

put(Key, Val) ->
    nklib_config:put(?APP, Key, Val).
