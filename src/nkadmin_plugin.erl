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

-module(nkadmin_plugin).

-export([plugin_deps/0, plugin_syntax/0, plugin_config/2]).


%% ===================================================================
%% Config callbacks
%% ===================================================================


plugin_deps() ->
    [nkservice_webserver].


plugin_syntax() ->
    #{
        nkadmin => #{
            webserver_url => fun nkservice_webserver_util:parse_url/1,
            webserver_opts => nkpacket_syntax:safe_syntax(),
            api_url => fun nkapi_util:parse_url/1,
            api_opts => nkpacket_syntax:safe_syntax()
        }
    }.


plugin_config(#{nkadmin:=NkAdmin}=Config, _Service) ->
    Config2 = case NkAdmin of
        #{webserver_url:=WebUrl} ->
            Priv = list_to_binary(code:priv_dir(nkadmin)),
            WebPath = <<Priv/binary, "/www">>,
            WebOpts = maps:get(webserver_opts, NkAdmin, #{}),
            WebObj = #{id => <<"nkadmin">>, url=>WebUrl, opts=>WebOpts, file_path=>WebPath},
            nkservice_util:add_config_obj(nkservice_webserver, WebObj, Config);
        _ ->
            Config
    end,
    Config3 = case NkAdmin of
        #{api_url:=ApiUrl} ->
            ApiOpts = maps:get(api_opts, NkAdmin, #{}),
            ApiObj = #{id => <<"nkadmin">>, url=>ApiUrl, opts=>ApiOpts},
            nkservice_util:add_config_obj(nkapi_server, ApiObj, Config2);
        _ ->
            Config2
    end,
    {ok, Config3};

plugin_config(Config, _Service) ->
    {ok, Config}.


