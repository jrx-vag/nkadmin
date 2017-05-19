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

-module(nkadmin_frame).
-export([get_frame/1, event/3]).

-include_lib("nkevent/include/nkevent.hrl").

-define(LLOG(Type, Txt, Args), "NkADMIN " ++ Txt, Args).



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
get_frame(State) ->
    case frame_domain(State) of
        {ok, Frame1} ->
            case frame_user(State) of
                {ok, Frame2} ->
                    {ok, Frame1 ++ Frame2, State};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
event(#nkevent{obj_id=ObjId}, Updates, State) ->
    case State of
        #{domain_id:=ObjId} ->
            {ok, Updates2} = frame_domain(State),
            {ok, Updates2++Updates, State};
        #{user_id:=ObjId} ->
            {ok, Updates2} = frame_user(State),
            {ok, Updates2++Updates, State};
        _ ->
            {ok, Updates, State}
    end.


%% ===================================================================
%% Internal
%% ===================================================================



%% @private
frame_domain( #{srv_id:=SrvId, domain_id:=DomainId}) ->
    case nkdomain:get_name(SrvId, DomainId) of
        {ok, #{name:=DomName, icon_id:=_DomIconId}} ->
            {ok, [
                #{
                    id => admin_frame_domain_name,
                    class => frameDomainName,
                    value => #{label => DomName}
                },
                #{
                    id => admin_frame_domain_icon,
                    class => frameDomainIcon,
                    value => #{icon => <<>>}
                }
            ]};
        {error, Error} ->
            {error, Error}
    end;

frame_domain(_) ->
    {ok, []}.


%% @private
frame_user(#{srv_id:=SrvId, user_id:=UserId}=State) ->
    case nkdomain_user_obj:get_name(SrvId, UserId) of
        {ok, #{<<"user">>:=#{name:=UserName, surname:=UserSurname, icon_id:=_UserIconId}}} ->
            {ok, [
                #{
                    id => admin_frame_user_name,
                    class => frameUserName,
                    value => #{label => <<UserName/binary, " ", UserSurname/binary>>}
                },
                #{
                    id => admin_frame_user_icon,
                    class => frameUserIcon,
                    value => #{icon => <<>>}
                },
                #{
                    id => frame_user_menu,
                    class => frameUserMenu,
                    value => #{
                        items => [
                            nkadmin_util:menu_item(admin_frame_user_menu_account, menuSimple, State),
                            #{class => frameUserMenuSeparator},
                            nkadmin_util:menu_item(admin_frame_user_menu_messages, menuSimple, State)
                        ]
                    }
                }
            ]};
        {error, Error} ->
            {error, Error}
    end;

frame_user(_) ->
    {ok, []}.

