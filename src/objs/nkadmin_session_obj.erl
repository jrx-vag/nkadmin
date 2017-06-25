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

-export([start/4]).
-export([switch_domain/3, element_action/5, get_data/4]).
-export([find_all/0]).
-export([object_info/0, object_parse/3, object_es_mapping/0,
         object_api_syntax/2, object_api_cmd/2]).
-export([object_init/1, object_stop/2, object_send_event/2,
         object_sync_op/3, object_async_op/2, object_handle_info/2]).
-export([object_admin_info/0]).
-export_type([session/0]).

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
    api_server_pid => pid(),
    domain_id => binary(),
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
    object_tags => #{ObjId::nkdomain:obj_id() => [Tag::term]},
    key_data => #{Key::binary() => term()},                         % Map client keys to data
    url => binary(),                                                % Defined from client
    url_key => #{Url::binary() => Key::binary()}
}.


%%-type meta() ::
%%    #{
%%        user_id => nkdomain:obj_id()
%%    }.


%%-type event() ::
%%    {update_elements, list()}.


%% ===================================================================
%% Public
%% ===================================================================


%% @doc Creates a new session
-spec start(nkservice:id(), nkdomain:id(), nkdomain:id(), start_opts()) ->
    {ok, nkdomain:obj_id(), pid()} | {error, term()}.

start(SrvId, DomainId, UserId, Opts) ->
    Obj = #{
        type => ?DOMAIN_ADMIN_SESSION,
        domain_id => DomainId,
        parent_id => UserId,
        created_by => UserId,
        active => true,
        ?DOMAIN_SESSION => #{}
    },
    case nkdomain_obj_make:create(SrvId, Obj, #{meta=>Opts}) of
        {ok, #obj_id_ext{obj_id=SessId, pid=Pid}, _} ->
            AdminDomainId = maps:get(domain_id, Opts, DomainId),
            case nkdomain_obj:sync_op(any, Pid, {?MODULE, switch_domain, AdminDomainId}) of
                {ok, Reply} ->
                    {ok, Reply#{session_id=>SessId}};
                {error, Error} ->
                    nkdomain:unload(any, Pid, start_error),
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.



%% @doc
switch_domain(SrvId, Id, DomainId) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, switch_domain, DomainId}).


%% @doc
element_action(SrvId, Id, ElementId, Action, Value) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, element_action, ElementId, Action, Value}).


%% @doc
get_data(SrvId, Id, ElementId, Data) ->
    nkdomain_obj:sync_op(SrvId, Id, {?MODULE, get_data, ElementId, Data}).


%% @private
find_all() ->
    nkdomain_domain_obj:find_all(root, root, #{filters=>#{type=>?DOMAIN_ADMIN_SESSION}}).




%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


%% @private
object_info() ->
    #{
        type => ?DOMAIN_ADMIN_SESSION,
        stop_after_disabled => true,
        remove_after_stop => true
    }.


%% @doc
object_admin_info() ->
    #{
        class => session,
        weight => 4000,
        tree_id => <<"domain_tree_sessions_admin.sessions">>
    }.


%% @private
object_es_mapping() ->
    not_indexed.


%% @private
object_parse(_SrvId, _Mode, _Obj) ->
    #{}.


%% @private
object_api_syntax(Cmd, Syntax) ->
    nkadmin_session_obj_syntax:api(Cmd, Syntax).


%% @private
object_api_cmd(Cmd, Req) ->
    nkadmin_session_obj_api:cmd(Cmd, Req).


%% @private
object_send_event(Event, State) ->
    nkadmin_session_obj_events:event(Event, State).


%% @private When the object is loaded, we make our cache
object_init(#?STATE{srv_id=SrvId, id=Id, obj=Obj, meta=Session}=State) ->
    %% TODO Link again if moved process
    #obj_id_ext{obj_id=SessId} = Id,
    #{created_by:=UserId} = Obj,
    #{api_server_pid:=ApiPid} = Session,
    Session2 = Session#{
        srv_id => SrvId,
        user_id => UserId
    },
    Session3 = maps:merge(#{language => <<"en">>}, Session2),
    ok = nkdomain_user_obj:register_session(SrvId, UserId, ?DOMAIN_ADMIN_SESSION, SessId, #{}),
    State2 = nkdomain_obj_util:link_to_api_server(?MODULE, ApiPid, State),
    State3 = State2#?STATE{meta=#{}, session=Session3},
    {ok, State3}.


%% @private
object_stop(_Reason, State) ->
    {ok, nkdomain_obj_util:unlink_from_api_server(?MODULE, State)}.


%% @private
object_sync_op({?MODULE, switch_domain, DomainId}, _From, State) ->
    case do_switch_domain(DomainId, State) of
        {ok, Reply, State2} ->
            {reply, {ok, Reply}, State2};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

object_sync_op({?MODULE, element_action, <<"url">>, updated, Url}, _From, State) ->
    #?STATE{session=Session} = State,
    {ElementId, Action, Value} = case nkadmin_util:get_url_key(Url, Session) of
        undefined ->
            lager:error("NKLOG URL NOT FOUND ~p", [Url]),
            {<<"url">>, updated, Url};
        Key ->
            {Key, selected, <<>>}
    end,
    case do_element_action(ElementId, Action, Value, State) of
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


%% @private We received an event from a subscribed object
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
do_switch_domain(Domain, #?STATE{srv_id=SrvId, session=Session}=State) ->
    case nkdomain_lib:load(SrvId, Domain) of
        #obj_id_ext{obj_id=DomainId, path=Path, type= ?DOMAIN_DOMAIN} ->
            case nkdomain_domain_obj:find_all_types(SrvId, DomainId, #{}) of
                {ok, _, TypeList, _Meta} ->
                    Types = [Type || {Type, _Counter} <- TypeList],
                    Session2 = Session#{
                        domain_id => DomainId,
                        domain_path => Path,
                        detail_path => Path,        % We can update it later with url
                        types => Types,
                        resources => [],
                        sessions => #{},
                        detail => #{},
                        object_tags => #{},
                        key_data => #{},
                        url_key => #{}
                    },
                    case do_get_domain_reply(SrvId, Session2) of
                        {ok, Updates, #{}=Session3} ->
                            OldPath = maps:get(domain_path, Session, <<>>),
                            subscribe_domain(OldPath, Session3),
                            State3 = State#?STATE{session=Session3},
                            io:format("UPDATES:\n\n~p\n", [Updates]),
                            {ok, #{elements=>lists:reverse(Updates)}, State3};
                        {error, Error, #{}=Session3} ->
                            State3 = State#?STATE{session=Session3},
                            {error, Error, State3}
                    end;
                {error, Error} ->
                    {error, Error, State}
            end;
        not_found ->
            {error, domain_unknown, State}
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
            do_get_domain_detail(SrvId, [Tree|Updates], Session2);
        {error, Error, Session2} ->
            {error, Error, Session2}
    end.


do_get_domain_detail(SrvId, Updates, Session) ->
    Url = case Session of
        #{url:=<<"#", Rest/binary>>} -> Rest;
        #{url:=Rest} -> Rest;
        _ -> maps:get(domain_path, Session)
    end,
    case nkadmin_util:get_url_key(Url, maps:remove(url, Session)) of
        undefined ->
            lager:error("NKLOG URL NOT FOUND ~p", [Url]),
            {Updates2, Session2} = nkadmin_util:update_detail(<<>>, #{}, Updates, Session),
            {ok, Updates2, Session2};
        Key ->
            case SrvId:admin_element_action(Key, selected, <<>>, Updates, Session) of
                {ok, Updates2, Session2} ->
                    {ok, Updates2, Session2};
                {error, Error, Session2} ->
                    lager:error("NKLOG ERROR IN URL ~p", [Error]),
                    {ok, Updates, Session2}
            end
    end.


%% @private Event from subscribed object
do_event(Event, #?STATE{srv_id=SrvId, session=Session}=State) ->
    {ok, UpdList, Session2} = SrvId:admin_event(Event, [], Session),
    State2 = State#?STATE{session=Session2},
    case UpdList of
        [] ->
            State2;
        _ ->
            send_event({update_elements, UpdList}, State2)
    end.


%% @private
send_event(Event, State) ->
    % TODO send directly to socket
    nkdomain_obj_util:event(Event, State).


%% @private
do_element_action(ElementId, Action, Value, State) ->
    #?STATE{srv_id=SrvId, session=Session} = State,
    case SrvId:admin_element_action(ElementId, Action, Value, [], Session) of
        {ok, UpdList, Session2} ->
            State2 = State#?STATE{session=Session2},
            case UpdList of
                [] ->
                    {ok, #{}, State2};
                _ ->
                    {ok, #{elements=>UpdList}, State2}
            end;
        {error, Error, Session2} ->
            State2 = State#?STATE{session=Session2},
            {reply, {error, Error}, State2}
    end.


%% @private
do_get_data(ElementId, Spec, State) ->
    #?STATE{srv_id=SrvId, session=Session} = State,
    case SrvId:admin_get_data(ElementId, Spec, Session) of
        {ok, Reply, Session2} ->
            State2 = State#?STATE{session=Session2},
            {ok, Reply, State2};
        {error, Error, Session2} ->
            State2 = State#?STATE{session=Session2},
            {error, Error, State2}
    end.



%% @private
subscribe_domain(OldPath, #{srv_id:=SrvId, domain_path:=NewPath}) ->
    case OldPath of
        <<>> ->
            ok;
        _ ->
            Reg = #{
                srv_id => SrvId,
                class => ?DOMAIN_EVENT_CLASS,
                domain => OldPath
            },
            nkevent:unreg(Reg)
    end,
    Reg2 = #{
        srv_id => SrvId,
        class => ?DOMAIN_EVENT_CLASS,
        domain => NewPath
    },
    nkevent:reg(Reg2).




%%%% @private
%%subscribe(Type, ObjId, #?STATE{srv_id=SrvId, session=Data}=State) ->
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
%%    State#?STATE{session=Data2}.
%%
%%
%%%% @private
%%unsubscribe(ObjId, #?STATE{session=Data}=State) ->
%%    #?MODULE{subs=Subs1} = Data,
%%    case maps:find(ObjId, Subs1) of
%%        {ok, Reg} ->
%%            nkevent:unreg(Reg),
%%            Subs2 = maps:remove(ObjId, Subs1),
%%            Data2 = Data#?MODULE{subs=Subs2},
%%            State#?STATE{session=Data2};
%%        error ->
%%        State
%%    end.


