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

-export([cmd/2]).

-include("nkadmin.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include_lib("nkservice/include/nkservice.hrl").


-define(ADMIN_DEF_EVENT_TYPES, [
    <<"update_elements">>,
    <<"unloaded">>
]).


%% ===================================================================
%% API
%% ===================================================================


%% @doc
cmd(<<"find">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    case get_user_id(Data, Req) of
        {ok, UserId} ->
            case nkadmin_session_obj:find(SrvId, UserId) of
                {ok, List} ->
                    {ok, #{sessions=>List}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"create">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    case get_user_id(Data, Req) of
        {ok, UserId} ->
            case nkadmin_session_obj:create(SrvId, UserId) of
                {ok, #{obj_id:=ObjId}, _Pid} ->
                    cmd(<<"start">>, Req#nkreq{data=Data#{id=>ObjId}});
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"start">>, #nkreq{data=#{id:=Id}=Data, user_id=UserId, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_DOMAIN, domain_id, Data, Req) of
        {ok, DomainId} ->
            Opts1 = maps:remove(id, Data),
            Opts2 = Opts1#{domain_id=>DomainId, user_id=>UserId, api_server_pid=>self()},
            case nkadmin_session_obj:start(SrvId, Id, Opts2) of
                {ok, ObjId, Reply} ->
                    Types = maps:get(events, Data, ?ADMIN_DEF_EVENT_TYPES),
                    Subs = #{
                        srv_id => SrvId,
                        class => ?DOMAIN_EVENT_CLASS,
                        subclass => ?DOMAIN_ADMIN_SESSION,
                        type => Types,
                        obj_id => ObjId
                    },
                    ok = nkapi_server:subscribe(self(), Subs),
                    UserMeta1 = nkdomain_api_util:add_user_meta(?DOMAIN_ADMIN_SESSION, ObjId, Req),
                    UserMeta2 = UserMeta1#{nkadmin_session_types=>Types},
                    {ok, Reply#{obj_id=>ObjId}, UserMeta2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"start">>, #nkreq{data=Data}=Req) ->
    case cmd(<<"find">>, Req) of
        {ok, #{sessions:=[#{<<"obj_id">>:=SessId}|_]}} ->
            cmd(<<"start">>, Req#nkreq{data=Data#{id=>SessId}});
        {ok, #{sessions:=[]}} ->
            {error, session_not_found};
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"stop">>, #nkreq{data=Data, srv_id=SrvId, user_meta=UserMeta}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, Req) of
        {ok, Id} ->
            UserMeta2 = case UserMeta of
                #{nkadmin_session_types:=Types} ->
                    Subs = #{
                        srv_id => SrvId,
                        class => ?DOMAIN_EVENT_CLASS,
                        subclass => ?DOMAIN_ADMIN_SESSION,
                        type => Types,
                        obj_id => Id
                    },
                    nkapi_server:unsubscribe(self(), Subs),
                    maps:remove(nkadmin_session_types, UserMeta);
                _ ->
                    UserMeta
            end,
            case nkadmin_session_obj:stop(SrvId, Id) of
                ok ->
                    {ok, #{}, UserMeta2};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"switch_domain">>, #nkreq{data=#{domain_id:=DomId}=Data, srv_id=SrvId}=Req) ->
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, Req) of
        {ok, Id} ->
            case nkadmin_session_obj:switch_domain(SrvId, Id, DomId) of
                {ok, Reply} ->
                    {ok, Reply};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"element_action">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    #{element_id:=ElementId, action:=Action} = Data,
    Value = maps:get(value, Data, <<>>),
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, Req) of
        {ok, Id} ->
            case nkadmin_session_obj:element_action(SrvId, Id, ElementId, Action, Value) of
                {ok, Reply} ->
                    {ok, Reply};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(<<"get_data">>, #nkreq{data=Data, srv_id=SrvId}=Req) ->
    #{element_id:=ElementId} = Data,
    case nkdomain_api_util:get_id(?DOMAIN_ADMIN_SESSION, Data, Req) of
        {ok, Id} ->
            case nkadmin_session_obj:get_data(SrvId, Id, ElementId, Data) of
                {ok, Reply} ->
                    {ok, Reply};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end;

cmd(Cmd, Req) ->
    nkdomain_obj_api:api(Cmd, ?DOMAIN_ADMIN_SESSION, Req).



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
get_user_id(#{user_id:=UserId}, _Req) ->
    {ok, UserId};
get_user_id(_, #nkreq{user_id=UserId}) when UserId /= <<>> ->
    {ok, UserId};
get_user_id(_Data, _Req) ->
    {error, missing_user_id}.
