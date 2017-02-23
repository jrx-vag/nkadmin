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

-module(nkadmin_session).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(gen_server).

-export([start/1, stop/2, switch_object/2, switch_domain/2]).
-export([find/1, do_call/2, do_cast/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2]).
-export_type([id/0, event/0]).

-include_lib("nkservice/include/nkservice.hrl").

-define(DEBUG(Txt, Args, State),
    case erlang:get(nkadmin_session_debug) of
        true -> ?LLOG(debug, Txt, Args, State);
        _ -> ok
    end).

-define(LLOG(Type, Txt, Args, State),
    lager:Type(
        [
            {admin_session_id, State#state.id},
            {user_id, State#state.user_id}
        ],
        "NkADMIN Session ~s (~s) "++Txt,
               [State#state.id, State#state.user_id | Args])).



%% ===================================================================
%% Types
%% ===================================================================

-type id() :: binary().

-type event() ::
    created                             |
    {stopped, nkservice:error()}        |
    destroyed.


-type config() ::
    #{
        srv_id => nkservice:id(),
        domain => nkdomain:domain(),
        user_id => nkservice:user_id(),
        user_session_id => nkservice:user_session(),
        register => nklib:link()
    }.


-type admin() ::
    config() |
    #{
        obj_id => nkdomain:obj_id(),
        frame => frame(),
        tree => tree(),
        detail => detail()
    }.

-type frame() :: #{}.

-type tree() :: #{}.

-type detail() :: #{}.



%% ===================================================================
%% Public functions
%% ===================================================================

%% @doc Starts a new session
-spec start(config()) ->
    {ok, id(), #{frame=>frame(), tree=>tree(), detail=>detail()}, pid()} |
    {error, term()}.

start(#{srv_id:=Srv}=Config) ->
    case nkservice_srv:get_srv_id(Srv) of
        {ok, SrvId} ->
            Config2 = Config#{srv_id:=SrvId},
            {AdminId, Config3} = nklib_util:add_id(admin_id, Config2, admin),
            {ok, Pid} = gen_server:start(?MODULE, [Config3], []),
            case do_call(Pid, get_all) of
                {ok, Data} ->
                    {ok, AdminId, Data, Pid};
                {error, Error} ->
                    stop(Pid, no_data),
                    {error, Error}
            end;
        not_found ->
            {error, service_not_found}
    end.


%% @doc Stops the server
-spec stop(id(), nkservice:error()) ->
    ok | {error, term()}.

stop(Id, Reason) ->
    do_cast(Id, {stop, Reason}).


%% @doc Points to a different object
-spec switch_object(id(), nkdomain:obj_id()) ->
    {ok, #{detail=>detail()}} | {error, term()}.

switch_object(Id, ObjId) ->
    do_call(Id, {switch_object, ObjId}).


%% @doc Points to a different domain
-spec switch_domain(id(), nkdomain:domain()) ->
    {ok, #{tree=>tree(), detail=>detail()}} | {error, term()}.

switch_domain(Id, Domain) ->
    do_call(Id, {switch_domain, Domain}).



% ===================================================================
%% gen_server behaviour
%% ===================================================================


-record(state, {
    id :: id(),
    srv_id :: nkservice:id(),
    user_id :: binary(),
    domain :: nkdomain:domain(),
    links :: nklib_links:links(),
    stop_reason = false :: false | nkservice:error(),
    admin :: admin()
}).


%% @private
-spec init(term()) ->
    {ok, tuple()}.

init([#{srv_id:=SrvId, admin_id:=AdminId, user:=User, domain:=Domain}=Config]) ->
    nklib_proc:put(?MODULE, AdminId),
    nklib_proc:put({?MODULE, AdminId}),
    Admin = Config#{
        obj_id => Domain
    },
    State1 = #state{
        id = AdminId,
        srv_id = SrvId,
        user_id = User,
        domain = Domain,
        admin = Admin
    },
    set_log(State1),
    nkservice_util:register_for_changes(SrvId),
    ?LLOG(info, "started (~p)", [self()], State1),
    State2 = case Admin of
         #{register:=Link} ->
             links_add(Link, reg, State1);
         _ ->
             State1
    end,
    State3 = event(created, State2),
    handle(nkadmin_session_init, [AdminId], State3).


%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {noreply, #state{}} | {reply, term(), #state{}} |
    {stop, Reason::term(), #state{}} | {stop, Reason::term(), Reply::term(), #state{}}.

handle_call(load, _From, State) ->
    #state{user_id=UserId, domain=Domain, admin=Admin} = State,
    {ok, Frame} = nkadmin_test_data:frame(UserId, Domain),
    {ok, Tree} = nkadmin_test_data:tree(UserId, Domain),
    {ok, Detail} = nkadmin_test_data:detail(UserId, Domain),
    Data = #{
        frame => Frame,
        tree => Tree,
        detail => Detail
    },
    Admin2 = maps:merge(Admin, Data),
    {reply, {ok, Data}, State#state{admin=Admin2}};

handle_call({switch_domain, Domain}, _From, #state{user_id=UserId, admin=Admin}=State) ->
    case nkadmin_test_data:tree(UserId, Domain) of
        {ok, Tree} ->
            case nkadmin_test_data:detail(UserId, Domain) of
                {ok, Detail} ->
                    Admin2 = Admin#{
                        obj_id := Domain,
                        tree := Tree,
                        detail := Detail
                    },
                    State2 = State#state{domain=Domain, admin=Admin2},
                    {reply, {ok, #{tree=>Tree, detail=>Detail}}, State2};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call({switch_object, ObjId}, _From, #state{user_id=UserId, admin=Admin}=State) ->
    case nkadmin_test_data:detail(UserId, ObjId) of
        {ok, Detail} ->
            Admin2 = Admin#{
                obj_id := ObjId,
                detail := Detail
            },
            {reply, {ok, Detail}, State#state{admin=Admin2}};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call(get_admin, _From, #state{admin=Admin}=State) ->
    {reply, {ok, Admin}, State};

handle_call({register, Link}, _From, State) ->
    ?DEBUG("proc registered (~p)", [Link], State),
    State2 = links_add(Link, reg, State),
    {reply, {ok, self()}, State2};

handle_call({unregister, Link}, _From, State) ->
    ?DEBUG("proc unregistered (~p)", [Link], State),
    State2 = links_remove(Link, State),
    {reply, ok, State2};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(get_links, _From, #state{links=Links}=State) ->
    {reply, Links, State};

handle_call(Msg, From, State) ->
    handle(nkadmin_session_handle_call, [Msg, From], State).


%% @private
-spec handle_cast(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_cast({stop, Reason}, State) ->
    do_stop(Reason, State);

handle_cast(Msg, State) ->
    handle(nkadmin_session_handle_cast, [Msg], State).


%% @private
-spec handle_info(term(), #state{}) ->
    {noreply, #state{}} | {stop, term(), #state{}}.

handle_info({'DOWN', Ref, process, _Pid, Reason}=Msg, State) ->
    #state{stop_reason=Stop} = State,
    case links_down(Ref, State) of
        {ok, _, _, State2} when Stop /= false ->
            {noreply, State2};
        {ok, Link, _Data, State2} ->
            case Reason of
                normal ->
                    ?DEBUG("linked ~p down (normal)", [Link], State);
                _ ->
                    ?LLOG(info, "linked ~p down (~p)", [Link, Reason], State)
            end,
            do_stop(registered_down, State2);
        not_found ->
            handle(nkadmin_session_handle_info, [Msg], State)
    end;

handle_info({nkservice_updated, _SrvId}, State) ->
    {noreply, set_log(State)};

handle_info(Msg, #state{}=State) ->
    handle(nkadmin_session_handle_info, [Msg], State).


%% @private
-spec code_change(term(), #state{}, term()) ->
    {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec terminate(term(), #state{}) ->
    ok.

terminate(Reason, #state{stop_reason=Stop}=State) ->
    case Stop of
        false ->
            Ref = nklib_util:uid(),
            ?LLOG(notice, "terminate error ~s: ~p", [Ref, Reason], State),
            {noreply, State2} = do_stop({internal_error, Ref}, State);
        _ ->
            State2 = State
    end,
    State3 = event(destroyed, State2),
    {ok, _State4} = handle(nkadmin_session_terminate, [Reason], State3),
    ok.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
set_log(#state{srv_id=SrvId}=State) ->
    Debug = case nkservice_util:get_debug_info(SrvId, ?MODULE) of
        {true, _} -> true;
        _ -> false
    end,
    put(nkadmin_session_debug, Debug),
    State.



%% @private
do_stop(Reason, #state{stop_reason=false}=State) ->
    ?DEBUG("stopped: ~p", [Reason], State),
    % Give time for possible registrations to success and capture stop event
    timer:sleep(100),
    State2 = event({stopped, Reason}, State),
    erlang:send_after(?SRV_DELAYED_DESTROY, self(), destroy),
    {noreply, State2#state{stop_reason=Reason}};

do_stop(_Reason, State) ->
    % destroy already sent
    {noreply, State}.


%% @private
event(Event, #state{id=Id}=State) ->
    ?DEBUG("sending 'event': ~p", [Event], State),
    State2 = links_fold(
        fun(Link, _Data, AccState) -> reg_event(Event, Link, AccState) end,
        State,
        State),
    {ok, State3} = handle(nkadmin_session_event, [Id, Event], State2),
    State3.


%% @private
reg_event(Event, Link, #state{id=Id}=State) ->
    {ok, State2} = handle(nkadmin_session_reg_event, [Id, Link, Event], State),
    State2.


%% @private
handle(Fun, Args, State) ->
    nklib_gen_server:handle_any(Fun, Args, State, #state.srv_id, #state.admin).


%% @private
do_call(AdminId, Msg) ->
    do_call(AdminId, Msg, 5000).


%% @private
do_call(AdminId, Msg, Timeout) ->
    case find(AdminId) of
        {ok, Pid} -> nkservice_util:call(Pid, Msg, Timeout);
        not_found -> {error, call_not_found}
    end.


%% @private
do_cast(AdminId, Msg) ->
    case find(AdminId) of
        {ok, Pid} -> gen_server:cast(Pid, Msg);
        not_found -> {error, call_not_found}
    end.

%% @private
find(Pid) when is_pid(Pid) ->
    {ok, Pid};

find(AdminId) ->
    case nklib_proc:values({?MODULE, AdminId}) of
        [{undefined, Pid}] -> {ok, Pid};
        [] -> not_found
    end.


%% @private
links_add(Link, Data, State) ->
    Pid = nklib_links:get_pid(Link),
    links_add(Link, Data, Pid, State).


%% @private
links_add(Link, Data, Pid, #state{links=Links}=State) ->
    State#state{links=nklib_links:add(Link, Data, Pid, Links)}.


%% @private
links_remove(Link, #state{links=Links}=State) ->
    State#state{links=nklib_links:remove(Link, Links)}.


%% @private
links_down(Ref, #state{links=Links}=State) ->
    case nklib_links:down(Ref, Links) of
        {ok, Link, Data, Links2} ->
            {ok, Link, Data, State#state{links=Links2}};
        not_found ->
            not_found
    end.


%% @private
links_fold(Fun, Acc, #state{links=Links}) ->
    nklib_links:fold_values(Fun, Acc, Links).

