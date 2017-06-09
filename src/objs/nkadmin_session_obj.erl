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

%% @doc State Object
-module(nkadmin_session_obj).
-behavior(nkdomain_obj).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([create/2, find/2, start/3, stop/2]).
-export([switch_domain/3, element_action/5, get_data/4]).
-export([object_get_info/0, object_mapping/0, object_parse/3,
         object_api_syntax/2, object_api_allow/3, object_api_cmd/2]).
-export([object_init/1, object_start/1, object_send_event/2,
         object_sync_op/3, object_async_op/2, object_handle_info/2]).
-export([object_admin_info/0]).
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


-type start_opts() :: #{
    language => binary(),
    caller_pid => pid(),
    domain_id => binary(),
    user_id => binary(),
    url => binary()
}.


-type session() :: #{
    srv_id => nkservice:id(),
    domain_id => nkdomain:obj_id(),
    domain_path => nkdomain:path(),
    detail_path => nkdomain:path(),
    user_id => nkdomain:obj_id(),
    language => binary(),
    types => [nkdomain:type()],
    resources => [nkdomain:type()],
    sessions => #{nkdomain:type() => integer()},
    detail => map(),
    objects => #{ObjId::nkdomain:obj_id() => [Tag::term]},
    keys => #{Key::binary() => term()}
}.


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
-spec start(nkservice:id(), nkdomain:id(), start_opts()) ->
    {ok, nkdomain:obj_id(), Reply::map()} | {error, term()}.

start(Srv, Id, Opts) ->
    case nkdomain_obj_lib:find(Srv, Id) of
        #obj_id_ext{pid=Pid} when is_pid(Pid) ->
            {error, session_already_present};
        _ ->
            case nkdomain_obj_lib:load(Srv, Id, #{}) of
                #obj_id_ext{pid=Pid} ->
                    nkdomain_obj:sync_op(Pid, {?MODULE, start, Opts});
                {error, Error} ->
                    {error, Error}
            end
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


%% @doc
get_data(Srv, Id, ElementId, Data) ->
    sync_op(Srv, Id, {?MODULE, get_data, ElementId, Data}).




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


-record(?MODULE, {
    session :: session(),
    api_pid :: pid(),
    meta = #{} :: map()
}).



%% @private
object_get_info() ->
    #{
        type => ?DOMAIN_ADMIN_SESSION
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 4000,
        tree_id => <<"domain_tree_sessions_admin">>
    }.


%% @private
object_mapping() ->
    #{}.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{}.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkadmin_session_obj_syntax:api(Cmd, Syntax).


%% @private
object_api_allow(_Cmd, _Req, Session) ->
    {true, Session}.


%% @private
object_api_cmd(Cmd, Req) ->
    nkadmin_session_obj_api:cmd(Cmd, Req).


%% @private
object_send_event(Event, State) ->
    nkadmin_session_obj_events:event(Event, State).


%% @private
%% We initialize soon in case of early terminate
object_init(State) ->
    {ok, State#?NKOBJ{data=#?MODULE{}}}.


%% @private When the object is loaded, we make our cache
object_start(#?NKOBJ{obj=Obj}=State) ->
    #{?DOMAIN_ADMIN_SESSION := _UserData} = Obj,
    {ok, State}.


%% @private
object_sync_op({?MODULE, start, Opts}, _From, State) ->
    #{api_server_pid:=ApiPid} = Opts,
    #?NKOBJ{data=#?MODULE{api_pid=OldPid}} = State,
    case OldPid /= undefined andalso ApiPid /= OldPid of
        true ->
            {reply, {error, session_already_present}, State};
        false ->
            State2 = nkdomain_obj_util:link_server_api(?MODULE, ApiPid, State),
            #?NKOBJ{obj_id=ObjId, srv_id=SrvId, data=Data} = State,
            Opts2 = maps:merge(#{language => <<"en">>}, Opts),
            Data2 = Data#?MODULE{session = Opts2#{srv_id=>SrvId}},
            State3 = State2#?NKOBJ{data=Data2},
            #{domain_id:=DomainId} = Opts2,
            case do_switch_domain(DomainId, State3) of
                {ok, Reply, State4} ->
                    {reply, {ok, ObjId, Reply}, State4};
                {error, Error, State4} ->
                    nkdomain_obj:unload(self(), internal_error),
                    {reply, {error, Error}, State4}
            end
    end;

object_sync_op({?MODULE, switch_domain, DomainId}, _From, State) ->
    case do_switch_domain(DomainId, State) of
        {ok, Reply, State2} ->
            {reply, {ok, Reply}, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, element_action, ElementId, Action, Value}, _From, State) ->
    case do_element_action(ElementId, Action, Value, State) of
        {ok, Reply, State2} ->
            {reply, {ok, Reply}, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, get_data, ElementId, Data}, _From, State) ->
    case do_get_data(ElementId, Data, State) of
        {ok, Reply, State2} ->
            {reply, {ok, Reply}, State2};
        {error, Error, State2} ->
            {reply, {error, Error}, State2}
    end;

object_sync_op(_Op, _From, _State) ->
    continue.


%% @private
object_async_op(_Op, _State) ->
    continue.


%% @private
object_handle_info({nkevent, #nkevent{type=Type}=Event}, State) ->
    case lists:member(Type, [<<"created">>, <<"updated">>, <<"deleted">>, <<"counter_updated">>]) of
        true ->
            {noreply, do_event(Event, State)};
        false ->
            {noreply, State}
    end;

object_handle_info(_Info, _State) ->
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
do_switch_domain(DomainId, #?NKOBJ{srv_id=SrvId, data=Data}=State) ->
    case nkdomain_obj_lib:load(SrvId, DomainId, #{}) of
        #obj_id_ext{obj_id=DomainObjId, path=Path, type= ?DOMAIN_DOMAIN} ->
            case nkdomain_domain_obj:find_all_types(SrvId, DomainId, #{}) of
                {ok, _, TypeList} ->
                    Types = [Type || {Type, _Counter} <- TypeList],
                    #?MODULE{session=Session1} = Data,
                    Session2 = Session1#{
                        domain_id => DomainObjId,
                        domain_path => Path,
                        detail_path => Path,
                        types => Types,
                        resources => [],
                        sessions => #{},
                        detail => #{},
                        objects => #{},
                        keys => #{}
                    },
                    case do_get_domain_reply(SrvId, Session2) of
                        {ok, Updates, #{}=Session3} ->
                            unsubscribe_domain(State),
                            subscribe_domain(Path, State),
                            Data3 = Data#?MODULE{session=Session3},
                            State3 = State#?NKOBJ{data=Data3},
                            {ok, #{elements=>lists:reverse(Updates)}, State3};
                        {error, Error, #{}=Session3} ->
                            Data3 = Data#?MODULE{session=Session3},
                            State3 = State#?NKOBJ{data=Data3},
                            {error, Error, State3}
                    end;
                {error, Error} ->
                    {error, Error, State}
            end;
        not_found ->
            {error, unknown_domain, State}
    end.


%% @private
do_get_domain_reply(SrvId, Session) ->
    do_get_domain_frame(SrvId, [], Session).


%% @private
do_get_domain_frame(SrvId, Updates, Session) ->
    case SrvId:admin_get_frame(Session) of
        {ok, Frame, Session2} ->
            do_get_domain_tree(SrvId, [Frame|Updates], Session2);
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.


%% @private
do_get_domain_tree(SrvId, Updates, Session) ->
    case SrvId:admin_get_tree(Session) of
        {ok, Tree, Session2} ->
            do_get_domain_url(SrvId, [Tree|Updates], Session2);
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.


%% @private
do_get_domain_url(SrvId, Updates, Session) ->
    case SrvId:admin_get_url(Session) of
        {ok, Url, BreadCrumb, Session2} ->
            do_get_domain_detail(SrvId, [Url, BreadCrumb|Updates], Session2);
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.


%% @private
do_get_domain_detail(SrvId, Updates, Session) ->
    case SrvId:admin_get_detail(Session) of
        {ok, Detail, Session2} ->
            {ok, [Detail|Updates], Session2};
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.



%% @private
do_event(Event, #?NKOBJ{srv_id=SrvId, data=Data}=State) ->
    #?MODULE{session=Session1} = Data,
    {ok, UpdList, Session2} = SrvId:admin_event(Event, [], Session1),
    Data2 = Data#?MODULE{session=Session2},
    State2 = State#?NKOBJ{data=Data2},
    case UpdList of
        [] ->
            State2;
        _ ->
            send_event({update_elements, UpdList}, State2)
    end.


%% @private
send_event(Event, State) ->
    nkdomain_obj_util:event(Event, State).


%% @private
do_element_action(ElementId, Action, Value, State) ->
    #?NKOBJ{srv_id=SrvId, data=#?MODULE{session=Session}=Data} = State,
    case SrvId:admin_element_action(ElementId, Action, Value, [], Session) of
        {ok, UpdList, Session2} ->
            Data2 = Data#?MODULE{session=Session2},
            State2 = State#?NKOBJ{data=Data2},
            case UpdList of
                [] ->
                    {ok, #{}, State2};
                _ ->
                    {ok, #{elements=>UpdList}, State2}
            end;
        {error, Error, Session2} ->
            Data2 = Data#?MODULE{session=Session2},
            State2 = State#?NKOBJ{data=Data2},
            {reply, {error, Error}, State2}
    end.


%% @private
do_get_data(ElementId, Spec, State) ->
    #?NKOBJ{srv_id=SrvId, data=#?MODULE{session=Session}=Data} = State,
    case SrvId:admin_get_data(ElementId, Spec, Session) of
        {ok, Reply, Session2} ->
            Data2 = Data#?MODULE{session=Session2},
            State2 = State#?NKOBJ{data=Data2},
            {ok, Reply, State2};
        {error, Error, Session2} ->
            Data2 = Data#?MODULE{session=Session2},
            State2 = State#?NKOBJ{data=Data2},
            {error, Error, State2}
    end.



%% @private
subscribe_domain(Path, #?NKOBJ{srv_id=SrvId}=State) ->
    Reg = #{
        srv_id => SrvId,
        class => ?DOMAIN_EVENT_CLASS,
        domain => Path
    },
    nkevent:reg(Reg),
    State.


%% @private
unsubscribe_domain(#?NKOBJ{srv_id=SrvId, data=#?MODULE{session=Session}}) ->
    case Session of
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
%%subscribe(Type, ObjId, #?NKOBJ{srv_id=SrvId, data=Data}=State) ->
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
%%    State#?NKOBJ{data=Data2}.
%%
%%
%%%% @private
%%unsubscribe(ObjId, #?NKOBJ{data=Data}=State) ->
%%    #?MODULE{subs=Subs1} = Data,
%%    case maps:find(ObjId, Subs1) of
%%        {ok, Reg} ->
%%            nkevent:unreg(Reg),
%%            Subs2 = maps:remove(ObjId, Subs1),
%%            Data2 = Data#?MODULE{subs=Subs2},
%%            State#?NKOBJ{data=Data2};
%%        error ->
%%        State
%%    end.


