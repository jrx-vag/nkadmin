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

%% @doc Session Object
-module(nkadmin_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2, find/2, start/4, stop/2]).
-export([switch_domain/3, start_detail/3, stop_detail/3]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/3, object_api_allow/4, object_api_cmd/4]).
-export([object_init/1, object_start/1, object_send_event/2,
         object_sync_op/3, object_async_op/2, object_handle_info/2]).
-export_type([meta/0, event/0]).

-include("nkadmin.hrl").
-include_lib("nkdomain/include/nkdomain.hrl").
-include_lib("nkdomain/include/nkdomain_debug.hrl").
-include_lib("nkevent/include/nkevent.hrl").


%% Period to find for inactive conversations
-define(CHECK_TIME, 5*60*1000).


%% ===================================================================
%% Types
%% ===================================================================


-type meta() ::
    #{
        user_id => nkdomain:obj_id()
    }.


-type event() :: none.
%%    {conversation_activated, ConvId::nkdomain:obj_id()} |
%%    {conversation_added, ConvId::nkdomain:obj_id()} |
%%    {conversation_removed, ConvId::nkdomain:obj_id()} |
%%    {member_added, ConvId::nkdomain:obj_id(), IsActive::boolean(), MemberId::nkdomain:obj_id()} |
%%    {member_removed, ConvId::nkdomain:obj_id(), IsActive::boolean(), MemberId::nkdomain:obj_id()} |
%%    {message_created, nkdomain:obj()} |
%%    {message_updated, nkdomain:obj()} |
%%    {message_deleted, nkdomain:obj_id()} |
%%    {unread_counter_updated, ConvId::nkdomain:obj_id(), integer()}.




%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec create(nkservice:id(), nkdomain:id()) ->
    {ok, nkdomain_obj_lib:make_and_create_reply(), pid()} | {error, term()}.

create(Srv, User) ->
    case nkdomain_obj_lib:find(Srv, User) of
        #obj_id_ext{obj_id = UserId} ->
            Obj = #{
                type => ?DOMAIN_ADMIN_SESSION,
                parent_id => UserId,
                ?DOMAIN_ADMIN_SESSION => #{}
            },
            nkdomain_obj_lib:make_and_create(Srv, <<>>, Obj, #{});
        _ ->
            {error, user_not_found}
    end.


%% @doc Tries to find a previous session for this user
-spec find(nkservice:id(), nkdomain:id()) ->
    {ok, [map()]} | {error, term()}.

find(Srv, User) ->
    case nkdomain_obj_lib:find(Srv, User) of
        #obj_id_ext{obj_id=UserId} ->
            Search = #{
                filters => #{
                    type => ?DOMAIN_ADMIN_SESSION,
                    parent_id => UserId
                },
                sort => [#{created_time => #{order => desc}}],
                fields => [created_time]
            },
            case nkdomain_store:find(Srv, Search) of
                {ok, _, [], _Meta} ->
                    {error, session_not_found};
                {ok, _, List, _Meta} ->
                    {ok, List};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            {error, user_not_found}
    end.


%% @doc Starts a new session, connected to the Caller
%% If the caller stops, we will stop the session
%% TODO: if the session is stopped, a final event should be sent
start(Srv, Id, DomainId, CallerPid) ->
    case nkdomain_obj_lib:load(Srv, Id, #{usage_link=>{CallerPid, ?MODULE}}) of
        #obj_id_ext{pid=Pid} ->
            case nkdomain_obj_lib:load(Srv, DomainId, #{}) of
                #obj_id_ext{obj_id=DomainObjId, type=?DOMAIN_DOMAIN} ->
                    nkdomain_obj:sync_op(Pid, {?MODULE, start, DomainObjId, CallerPid});
                #obj_id_ext{} ->
                    {error, domain_unknown};
                {error, object_not_found} ->
                    {error, domain_unknown};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
stop(Srv, Id) ->
    nkdomain_obj_lib:unload(Srv, Id, user_stop, session_not_found).


%% @doc
switch_domain(Srv, Id, DomainId) ->
    case nkdomain_obj_lib:load(Srv, DomainId, #{}) of
        #obj_id_ext{obj_id=DomainObjId, type=?DOMAIN_DOMAIN} ->
            sync_op(Srv, Id, {?MODULE, switch_domain, DomainObjId});
        #obj_id_ext{} ->
            {error, domain_unknown};
        {error, object_not_found} ->
            {error, domain_unknown};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
start_detail(Srv, Id, DetailId) ->
    case nkdomain_obj_lib:load(Srv, DetailId, #{}) of
        #obj_id_ext{obj_id=ObjId} ->
            sync_op(Srv, Id, {?MODULE, start_detail, ObjId});
        {error, detail_not_found} ->
            {error, domain_unknown};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
stop_detail(Srv, Id, ObjId) ->
    sync_op(Srv, Id, {?MODULE, stop_detail, ObjId}).


%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


-record(?MODULE, {
    domain_id :: binary(),
    detail_ids = [] :: [binary()],
    api_pids = [] :: [pid()],
    user_config = #{} :: map(),
    meta = #{} :: map()
}).



%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_ADMIN_SESSION
    }.


%% @private
object_mapping() ->
    #{}.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{}.


%% @private
object_api_syntax(Sub, Cmd, Syntax) ->
    nkadmin_session_obj_syntax:api(Sub, Cmd, Syntax).


%% @private
object_api_allow(_Sub, _Cmd, _Data, State) ->
    {true, State}.


%% @private
object_api_cmd(Sub, Cmd, Req, State) ->
    nkadmin_session_obj_api:cmd(Sub, Cmd, Req, State).


%% @private
object_send_event(Event, Session) ->
    nkadmin_session_obj_events:event(Event, Session).


%% @private
%% We initialize soon in case of early terminate
object_init(Session) ->
    {ok, Session#obj_session{data=#?MODULE{}}}.


%% @private When the object is loaded, we make our cache
object_start(#obj_session{obj=Obj}=Session) ->
    #{?DOMAIN_ADMIN_SESSION := UserData} = Obj,
    Data = #?MODULE{
        user_config = UserData
    },
    {ok, Session#obj_session{data=Data}}.


%% @private
object_sync_op({?MODULE, start, Domain, Pid}, _From, #obj_session{obj_id=ObjId, data=Data}=Session) ->
    #?MODULE{api_pids=Pids} = Data,
    Data2 = Data#?MODULE{
        domain_id = Domain,
        detail_ids = [Domain],
        api_pids=[Pid|Pids]},
    Session2 = Session#obj_session{data=Data2},
    Reply = #{
        domain_id => Domain,
        detail_id => Domain,
        frame => do_get_frame(Session2),
        tree => do_get_tree(Session),
        detail => do_get_detail(Domain, Session)
    },
    {reply, {ok, ObjId, Reply}, Session2};

object_sync_op({?MODULE, switch_domain, Domain}, _From, #obj_session{data=Data}=Session) ->
    #?MODULE{domain_id=_OldDomain, detail_ids=_OldObjIds} = Data,
    Data2 = Data#?MODULE{
        domain_id = Domain,
        detail_ids = [Domain]
    },
    Session2 = Session#obj_session{data=Data2},
    Reply = #{
        domain_id => Domain,
        detail_id => Domain,
        tree => do_get_tree(Session),
        detail => do_get_detail(Domain, Session)
    },
    {reply, {ok, Reply}, Session2};

object_sync_op({?MODULE, start_detail, ObjId}, _From, #obj_session{data=Data}=Session) ->
    #?MODULE{detail_ids=ObjIds} = Data,
    case lists:member(ObjId, ObjIds) of
        true ->
            {reply, {error, already_started}, Session};
        false ->
            Data2 = Data#?MODULE{detail_ids = [ObjId|ObjIds]},
            Session2 = Session#obj_session{data=Data2},
            Reply = #{
                detail_id => ObjId,
                detail => do_get_detail(ObjId, Session)
            },
            {reply, {ok, Reply}, Session2}
    end;

object_sync_op({?MODULE, stop_detail, ObjId}, _From, #obj_session{data=Data}=Session) ->
    #?MODULE{detail_ids=ObjIds} = Data,
    case lists:member(ObjId, ObjIds) of
        true ->
            ObjIds2 = ObjIds -- [ObjId],
            Data2 = Data#?MODULE{detail_ids = ObjIds2},
            Session2 = Session#obj_session{data=Data2},
            {reply, {ok, #{}}, Session2};
        false ->
            {reply, {error, not_started}, Session}
    end;

object_sync_op({?MODULE, stop_object, _ObjId}, _From, Session) ->
    {reply, {error, not_found}, Session};

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op(_Op, _Session) ->
    continue.


object_handle_info(_Info, _Session) ->
    continue.


%% ===================================================================
%% Internal
%% ===================================================================

%% @private
sync_op(Srv, Id, Op) ->
    nkdomain_obj_lib:sync_op(Srv, Id, ?DOMAIN_ADMIN_SESSION, Op, session_not_found).


%%%% @private
%%async_op(Srv, Id, Op) ->
%%    nkdomain_obj_lib:async_op(Srv, Id, ?DOMAIN_ADMIN_SESSION, Op, session_not_found).


%% @private
do_get_frame(_Session) ->
    #{}.


%% @private
do_get_tree(_Session) ->
    #{}.


%% @private
do_get_detail(_ObjId, _Session) ->
    #{}.