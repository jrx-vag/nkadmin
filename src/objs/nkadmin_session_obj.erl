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
-export([object_admin_tree/3]).
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
    {update_elements, list()}.


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
                    State = #{user_id=>UserId, language=>Language},
                    nkdomain_obj:sync_op(Pid, {?MODULE, start, DomainObjId, State, CallerPid});
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
    state :: nkadmin_callbacks:state(),
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
object_sync_op({?MODULE, start, DomainId, State, Pid}, _From, Session) ->
    #obj_session{obj_id=ObjId, srv_id=SrvId, data=Data} = Session,
    #?MODULE{api_pids=Pids} = Data,
    Data2 = Data#?MODULE{
        state = State#{srv_id=>SrvId},
        api_pids = [Pid|Pids]
    },
    Session2 = Session#obj_session{data=Data2},
    case do_switch_domain(DomainId, Session2) of
        {ok, Reply, Session3} ->
            {reply, {ok, ObjId, Reply}, Session3};
        {error, Error} ->
            nkdomain_obj:unload(self(), internal_error),
            {reply, {error, Error}, Session2}
    end;

object_sync_op({?MODULE, switch_domain, DomainId}, _From, Session) ->
    case do_switch_domain(DomainId, Session) of
        {ok, Reply, Session2} ->
            {reply, {ok, Reply}, Session2};
        {error, Error} ->
            {reply, {error, Error}, Session}
    end;

object_sync_op({?MODULE, element_action, ElementId, Action, Value}, _From, Session) ->
    case do_element_action(ElementId, Action, Value, Session) of
        {ok, Reply, Session2} ->
            {reply, {ok, Reply}, Session2};
        {error, Error} ->
            {reply, {error, Error}, Session}
    end;

object_sync_op(_Op, _From, _Session) ->
    continue.


%% @private
object_async_op(_Op, _Session) ->
    continue.


%% @private
object_handle_info({nkevent, #nkevent{type=Type}=Event}, Session) ->
    case lists:member(Type, [<<"created">>, <<"updated">>, <<"deleted">>, <<"counter_updated">>]) of
        true ->
            {noreply, do_event(Event, Session)};
        false ->
            {noreply, Session}
    end;

object_handle_info(_Info, _Session) ->
    continue.


%% @doc
object_admin_tree(Category, List, State) ->
    nkdomain_admin:add_tree_session(Category, ?DOMAIN_ADMIN_SESSION, ?MODULE,
                                    domain_tree_sessions_admin, 4000, List, State).


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
do_switch_domain(DomainId, #obj_session{srv_id=SrvId, data=Data}=Session) ->
    case nkdomain_obj_lib:load(SrvId, DomainId, #{}) of
        #obj_id_ext{obj_id=DomainObjId, path=Path, type= ?DOMAIN_DOMAIN} ->
            unsubscribe_domain(Session),
            case nkdomain_domain_obj:find_all_types(SrvId, DomainId, #{}) of
                {ok, _, TypeList} ->
                    #?MODULE{state=State1} = Data,
                    State2 = State1#{
                        domain_id => DomainObjId,
                        domain_path => Path,
                        types => maps:from_list(TypeList),
                        session_types => #{},
                        elements => #{}
                    },
                    subscribe_domain(Path, Session),
                    {ok, Frame, State3} = do_get_frame(State2, Session),
                    {ok, Tree, State4} = do_get_tree(State3, Session),
                    {ok, Detail, State5} = do_get_detail(State4, Session),
                    Data5 = Data#?MODULE{state=State5},
                    Session5 = Session#obj_session{data=Data5},
                    Reply = #{
                        frame => Frame,
                        tree => Tree,
                        detail => Detail
                    },
                    {ok, Reply, Session5};
                {error, Error} ->
                    {error, Error}
            end;
        not_found ->
            {error, unknown_domain}
    end.


%% @private
do_event(Event, #obj_session{srv_id=SrvId, data=Data}=Session) ->
    #?MODULE{state=State1} = Data,
    {ok, UpdList, State2} = SrvId:admin_event(Event, [], State1),
    Data2 = Data#?MODULE{state=State2},
    Session2 = Session#obj_session{data=Data2},
    case UpdList of
        [] ->
            Session2;
        _ ->
            send_event({update_elements, UpdList}, Session2)
    end.


%% @private
do_get_frame(State, #obj_session{srv_id=SrvId}) ->
    {ok, Frame, State2} = SrvId:admin_get_frame(State),
    {ok, Frame, State2}.


%% @private
do_get_tree(State, #obj_session{srv_id=SrvId}) ->
    {ok, Tree, State2} = SrvId:admin_get_tree(State),
    {ok, Tree, State2}.


%% @private
do_get_detail(State, #obj_session{srv_id=SrvId}) ->
    {ok, Detail, State2} = SrvId:admin_get_detail(State),
    {ok, Detail, State2}.


%% @private
send_event(Event, Session) ->
    nkdomain_obj_util:event(Event, Session).


%% @private
do_element_action(ElementId, Action, Value, Session) ->
    #obj_session{srv_id=SrvId, data=#?MODULE{state=State}=Data} = Session,
    case SrvId:admin_element_action(ElementId, Action, Value, State) of
        {ok, Reply, State2} ->
            Data2 = Data#?MODULE{state=State2},
            {ok, Reply, Session#obj_session{data=Data2}};
        {error, Error} ->
            {error, Error}
    end.


%% @private
subscribe_domain(Path, #obj_session{srv_id=SrvId}=Session) ->
    Reg = #{
        srv_id => SrvId,
        class => ?DOMAIN_EVENT_CLASS,
        domain => Path
    },
    nkevent:reg(Reg),
    Session.


%% @private
unsubscribe_domain(#obj_session{srv_id=SrvId, data=#?MODULE{state=State}}) ->
    case State of
        #{domain_path:=DomainPath} ->
            Reg = #{
                srv_id => SrvId,
                class => ?DOMAIN_EVENT_CLASS,
                domain => DomainPath
            },
            nkevent:unreg(Reg);
        _ ->
            ok
    end.


%%%% @private
%%subscribe(Type, ObjId, #obj_session{srv_id=SrvId, data=Data}=Session) ->
%%    Reg = #{
%%        srv_id => SrvId,
%%        class => ?DOMAIN_EVENT_CLASS,
%%        subclass => Type,
%%        obj_id => ObjId
%%    },
%%    {ok, [_Pid]} = nkevent:reg(Reg),
%%    #?MODULE{subs=Subs1} = Data,
%%    Subs2 = Subs1#{ObjId => Reg},
%%    Data2 = Data#?MODULE{subs=Subs2},
%%    Session#obj_session{data=Data2}.
%%
%%
%%%% @private
%%unsubscribe(ObjId, #obj_session{data=Data}=Session) ->
%%    #?MODULE{subs=Subs1} = Data,
%%    case maps:find(ObjId, Subs1) of
%%        {ok, Reg} ->
%%            nkevent:unreg(Reg),
%%            Subs2 = maps:remove(ObjId, Subs1),
%%            Data2 = Data#?MODULE{subs=Subs2},
%%            Session#obj_session{data=Data2};
%%        error ->
%%        Session
%%    end.


