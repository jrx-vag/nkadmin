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

-module(nkadmin_session_events).

-export([event/3, event_session_down/3]).

-include_lib("nkservice/include/nkservice.hrl").



%% ===================================================================
%% Callbacks
%% ===================================================================

%% @private
-spec event(nkadmin:id(), nkadmin:event(), nkadmin:admin()) ->
    {ok, nkadmin:admin()}.

event(SessId, created, Admin) ->
    Data = nkadmin_session_syntax:get_info(Admin),
    send_event(SessId, created, Data, Admin);


% The 'destroyed' event is only internal, to remove things, etc.
event(SessId, {stopped, Reason}, #{srv_id:=SrvId}=Admin) ->
    {Code, Txt} = nkservice_util:error_code(SrvId, Reason),
    send_event(SessId, destroyed, #{code=>Code, reason=>Txt}, Admin);

event(_SessId, _Event, Admin) ->
    {ok, Admin}.


%% @private
-spec event_session_down(nkservice:id(), nkadmin:id(), term()) ->
    ok.

event_session_down(SrvId, SessId, ConnId) ->
    {Code, Txt} = nkservice_util:error_code(SrvId, process_down),
    Fake = #{
        srv_id => SrvId,
        user_session_id => ConnId,
        session_events => [<<"destroyed">>]
    },
    send_event(SessId, destroyed, #{code=>Code, reason=>Txt}, Fake).


%% ===================================================================
%% Internal
%% ===================================================================


%% @doc Sends an event
-spec send_event(nkadmin:id(), nkservice_events:type(),
    nkservice_events:body(), nkadmin:admin()) ->
    ok.

%% @private
send_event(SessId, Type, Body, #{srv_id:=SrvId}=Admin) ->
    Event = #event{
        srv_id = SrvId,
        class = <<"admin">>,
        subclass = <<"session">>,
        type = nklib_util:to_binary(Type),
        obj_id = SessId,
        body = Body
    },
    send_direct_event(Event, Admin),
    nkservice_events:send(Event),
    {ok, Admin}.


%% @private
send_direct_event(#event{type=Type, body=Body}=Event, Admin) ->
    case Admin of
        #{session_events:=Events, user_session_id:=ConnId} ->
            case lists:member(Type, Events) of
                true ->
                    Event2 = case Admin of
                                 #{session_events_body:=Body2} ->
                                     Event#event{body=maps:merge(Body, Body2)};
                                 _ ->
                                     Event
                             end,
                    nkservice_api_server:event(ConnId, Event2);
                false ->
                    ok
            end;
        _ ->
            ok
    end.

