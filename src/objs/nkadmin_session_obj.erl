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

-export([create/2, find/2, start/6, stop/2]).
-export([switch_domain/3, element_action/5]).
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


-type event() ::
    {frame_updated, list()}.



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
start(Srv, Id, DomainId, UserId, Language, CallerPid) ->
    case nkdomain_obj_lib:load(Srv, Id, #{usage_link=>{CallerPid, ?MODULE}}) of
        #obj_id_ext{pid=Pid} ->
            case nkdomain_obj_lib:load(Srv, DomainId, #{}) of
                #obj_id_ext{obj_id=DomainObjId, type=?DOMAIN_DOMAIN} ->
                    nkdomain_obj:sync_op(Pid, {?MODULE, start, DomainObjId, UserId, Language, CallerPid});
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
element_action(Srv, Id, ElementId, Action, Value) ->
    sync_op(Srv, Id, {?MODULE, element_action, ElementId, Action, Value}).



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


-record(?MODULE, {
    user_id :: binary(),
    language :: binary(),
    domain_id :: binary(),
    elements = #{},
    subs = #{},
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
object_sync_op({?MODULE, start, DomainId, UserId, Language, Pid}, _From, Session) ->
    #obj_session{obj_id=ObjId, data=Data} = Session,
    #?MODULE{api_pids=Pids} = Data,
    Data2 = Data#?MODULE{
        user_id = UserId,
        language = Language,
        domain_id = DomainId,
        api_pids=[Pid|Pids]
    },
    Session2 = Session#obj_session{data=Data2},
    Session3 = subscribe(?DOMAIN_USER, UserId, Session2),
    FrameData = #{domain_id=>DomainId, user_id=>UserId, user_menu=>true},
    case do_switch_domain(FrameData, Session3) of
        {ok, Reply, Session4} ->
            {reply, {ok, ObjId, Reply}, Session4};
        {error, Error} ->
            nkdomain_obj:unload(self(), internal_error),
            {reply, {error, Error}, Session3}
    end;

object_sync_op({?MODULE, switch_domain, DomainId}, _From, Session) ->
    case do_switch_domain(#{domain_id=>DomainId}, Session) of
        {ok, Reply, Session2} ->
            {reply, {ok, Reply}, Session2};
        {error, Error} ->
            {reply, {error, Error}, Session}
    end;

object_sync_op({?MODULE, element_action, _ElementId, _Action, _Value}, _From, #obj_session{data=Data}=Session) ->
    #?MODULE{elements=_Elements} = Data,
    {reply, {ok, #{}}, Session};

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op(_Op, _Session) ->
    continue.


%% @private
object_handle_info({nkevent, #nkevent{obj_id=ObjId}=Event}, #obj_session{data=Data}=Session) ->
    Session2 = case Data of
        #?MODULE{domain_id=ObjId} ->
            do_domain_event(Event, Session);
        #?MODULE{user_id=ObjId} ->
            do_user_event(Event, Session);
        _ ->
            do_event(Event, Session)
    end,
    {noreply, Session2};

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
do_switch_domain(#{domain_id:=DomainId}=FrameData, Session) ->
    case do_get_frame(FrameData, Session) of
        {ok, Frame, Session2} ->
            #obj_session{data=#?MODULE{domain_id=OldDomainId}=Data} = Session2,
            Data2 = Data#?MODULE{domain_id=DomainId},
            Session3 = unsubscribe(OldDomainId, Session#obj_session{data=Data2}),
            Session4 = subscribe(?DOMAIN_DOMAIN, DomainId, Session3),
            Reply = #{
                frame => Frame
            },
            {ok, Reply, Session4};
        {error, Error} ->
            ?LLOG(warning, "error loading frame: ~p", [Error], Session),
            {error, internal_error}
    end.


%% @private
do_domain_event(#nkevent{type=Type, obj_id=DomainId}, Session)
        when Type == <<"object_updated">> ->
    do_update_frame(#{domain_id=>DomainId}, Session);

do_domain_event(_, Session) ->
    Session.


%% @private
do_user_event(#nkevent{type=Type, obj_id=UserId}, Session)
    when Type == <<"object_updated">> ->
    do_update_frame(#{user_id=>UserId}, Session);

do_user_event(_, Session) ->
    Session.


%% @private
do_event(_Event, Session) ->
    Session.



%% @private
do_get_frame(FrameData, #obj_session{srv_id=SrvId, data=Data}=Session) ->
    #?MODULE{language=Language} =Data,
    case SrvId:admin_get_frame(SrvId, FrameData#{language=>Language}) of
        {ok, Frame} ->
            {ok, Frame, Session};
        {error, Error} ->
            {error, Error}
    end.


%% @private
do_update_frame(FrameData, Session) ->
    case do_get_frame(FrameData, Session) of
        {ok, Frame, Session2} ->
            send_event({frame_updated, Frame}, Session2);
        {error, Error} ->
            ?LLOG(warning, "error calling do_get_frame: ~p (~s)", [Error, FrameData], Session),
            Session
    end.


%% @private
send_event(Event, Session) ->
    nkdomain_obj_util:event(Event, Session).


%% @private
subscribe(Type, ObjId, #obj_session{srv_id=SrvId, data=Data}=Session) ->
    Reg = #{
        srv_id => SrvId,
        class => ?DOMAIN_EVENT_CLASS,
        subclass => Type,
        obj_id => ObjId
    },
    {ok, [_Pid]} = nkevent:reg(Reg),
    #?MODULE{subs=Subs1} = Data,
    Subs2 = Subs1#{ObjId => Reg},
    Data2 = Data#?MODULE{subs=Subs2},
    Session#obj_session{data=Data2}.


%% @private
unsubscribe(ObjId, #obj_session{data=Data}=Session) ->
    #?MODULE{subs=Subs1} = Data,
    case maps:find(ObjId, Subs1) of
        {ok, Reg} ->
            nkevent:unreg(Reg),
            Subs2 = maps:remove(ObjId, Subs1),
            Data2 = Data#?MODULE{subs=Subs2},
            Session#obj_session{data=Data2};
        error ->
        Session
    end.


