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

%% @doc Session Object API
-module(nkadmin_session_obj_api).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([cmd/4]).

-include("nkadmin.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkapi/include/nkapi.hrl").


-define(ADMIN_DEF_EVENT_TYPES, [
    <<"unloaded">>
]).


%% ===================================================================
%% API
%% ===================================================================


%% @doc
cmd('', find, #nkapi_req{data=Data}, #{srv_id:=SrvId}=State) ->
    case get_user_id(Data, State) of
        {ok, UserId} ->
            case nkadmin_session_obj:find(SrvId, UserId) of
                {ok, List} ->
                    {ok, #{sessions=>List}, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', create, #nkapi_req{data=Data}=Req, #{srv_id:=SrvId}=State) ->
    case get_user_id(Data, State) of
        {ok, UserId} ->
            case nkadmin_session_obj:create(SrvId, UserId) of
                {ok, #{obj_id:=ObjId}, _Pid} ->
                    cmd('', start, Req#nkapi_req{data=Data#{id=>ObjId}}, State);
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', start, #nkapi_req{data=#{id:=Id}=Data}, #{srv_id:=SrvId, user_id:=UserId}=State) ->
    case nkdomain_api_util:get_domain(Data, State) of
        {ok, Domain} ->
            case nkadmin_session_obj:start(SrvId, Id, Domain, UserId, self()) of
                {ok, ObjId, Reply} ->
                    State2 = nkdomain_api_util:add_id(?DOMAIN_ADMIN_SESSION, ObjId, State),
                    Types = maps:get(events, Data, ?ADMIN_DEF_EVENT_TYPES),
                    Subs = #{
                        srv_id => SrvId,
                        class => ?DOMAIN_EVENT_CLASS,
                        subclass => ?DOMAIN_ADMIN_SESSION,
                        type => Types,
                        obj_id => ObjId
                    },
                    ok = nkapi_server:subscribe(self(), Subs),
                    {ok, Reply#{obj_id=>ObjId}, State2#{nkadmin_session_types=>Types}};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', start, #nkapi_req{data=Data}=Req, State) ->
    case cmd('', find, Req, State) of
        {ok, #{sessions:=[#{<<"obj_id">>:=SessId}|_]}, State2} ->
            cmd('', start, Req#nkapi_req{data=Data#{id=>SessId}}, State2);
        {ok, #{sessions:=[]}, State2} ->
            {error, session_not_found, State2};
        {error, Error, State2} ->
            {error, Error, State2 }
    end;

cmd('', stop, #nkapi_req{data=Data}, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, State) of
        {ok, Id} ->
            State2 = case State of
                #{nkadmin_session_types:=Types} ->
                    Subs = #{
                        srv_id => SrvId,
                        class => ?DOMAIN_EVENT_CLASS,
                        subclass => ?DOMAIN_ADMIN_SESSION,
                        type => Types,
                        obj_id => Id
                    },
                    nkapi_server:unsubscribe(self(), Subs),
                    maps:remove(nkadmin_session_types, State);
                _ ->
                    State
            end,
            case nkadmin_session_obj:stop(SrvId, Id) of
                ok ->
                    {ok, #{}, State2};
                {error, Error} ->
                    {error, Error, State2}
            end;
        Error ->
            Error
    end;

cmd('', switch_domain, #nkapi_req{data=#{domain_id:=DomId}=Data}, #{srv_id:=SrvId}=State) ->
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, State) of
        {ok, Id} ->
            case nkadmin_session_obj:switch_domain(SrvId, Id, DomId) of
                {ok, Reply} ->
                    {ok, Reply, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd('', element_action, #nkapi_req{data=Data}, #{srv_id:=SrvId}=State) ->
    #{element_id:=ElementId, action:=Action} = Data,
    Value = maps:get(value, Data, <<>>),
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, State) of
        {ok, Id} ->
            case nkadmin_session_obj:element_action(SrvId, Id, ElementId, Action, Value) of
                {ok, Reply} ->
                    {ok, Reply, State};
                {error, Error} ->
                    {error, Error, State}
            end;
        Error ->
            Error
    end;

cmd(Sub, Cmd, Req, State) ->
    nkdomain_obj_api:api(Sub, Cmd, Req, ?DOMAIN_ADMIN_SESSION, State).



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
get_user_id(#{user_id:=UserId}, _State) ->
    {ok, UserId};
get_user_id(_, #{user_id:=UserId}) when UserId /= <<>> ->
    {ok, UserId};
get_user_id(_Data, State) ->
    {error, missing_user_id, State}.
