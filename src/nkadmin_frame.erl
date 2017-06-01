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
-export([get_frame/1, event/3, element_action/5]).

-include_lib("nkevent/include/nkevent.hrl").

-define(LLOG(Type, Txt, Args), "NkADMIN " ++ Txt, Args).



%% ===================================================================
%% Public
%% ===================================================================

%% @doc
get_frame(State) ->
    case frame_domain(State) of
        {ok, Items1, State2} ->
            case frame_user(State2) of
                {ok, Items2, State3} ->
                    Value = #{
                        class => frame,
                        value => #{items => Items1 ++ Items2}
                    },
                    {ok, Value, State3};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error, State}
    end.


%% @doc
event(#nkevent{type = <<"updated">>, obj_id=ObjId}, Updates, State) ->
    case State of
        #{domain_id:=ObjId} ->
            {ok, Items, State2} = frame_domain(State),
            {ok, Items++Updates, State2};
        #{user_id:=ObjId} ->
            {ok, Items, State2} = frame_user(State),
            {ok, Items++Updates, State2};
        _ ->
            {ok, Updates, State}
    end;

event(#nkevent{type = <<"deleted">>, obj_id=ObjId}, Updates, State) ->
    case State of
        #{domain_id:=ObjId} ->
            nkdomain_obj:unload(self(), domain_deleted),
            {ok, Updates, State};
        #{user_id:=ObjId} ->
            nkdomain_obj:unload(self(), user_deleted),
            {ok, Updates, State};
        _ ->
            {ok, Updates, State}
    end;

event(_Event, Updates, State) ->
    {ok, Updates, State}.


%% @doc
element_action(_ElementId, _Id, _Value, Updates, State) ->
    {ok, Updates, State}.


%% ===================================================================
%% Internal
%% ===================================================================



%% @private
frame_domain(#{srv_id:=SrvId, domain_id:=DomainId}=State) ->
    case nkdomain:get_name(SrvId, DomainId) of
        {ok, #{name:=DomName, icon_id:=_DomIconId}} ->
            Items = [
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
            ],
            State2 = nkadmin_util:add_object_tag(DomainId, nkadmin_frame_domain, State),
            {ok, Items, State2};
        {error, Error} ->
            {error, Error}
    end.


%% @private
frame_user(#{srv_id:=SrvId, user_id:=UserId}=State) ->
    case nkdomain_user_obj:get_name(SrvId, UserId) of
        {ok, #{<<"user">>:=#{name:=UserName, surname:=UserSurname}, icon_id:=UserIconId}} ->
            Items = [
                #{
                    id => admin_frame_user_name,
                    class => frameUserName,
                    value => #{label => <<UserName/binary, " ", UserSurname/binary>>}
                },
                #{
                    id => admin_frame_user_icon,
                    class => frameUserIcon,
                    value => #{icon_id => UserIconId}
                },
                #{
                    id => admin_frame_user_menu,
                    class => frameUserMenu,
                    value => #{
                        icon => user,
                        items => [
                            nkadmin_util:menu_item(admin_frame_user_menu_account, menuEntry, #{icon=>gear}, State),
                            #{class => frameUserMenuSeparator},
                            nkadmin_util:menu_item(admin_frame_user_menu_messages, menuEntry, #{icon=>comments}, State),
                            #{class => frameUserMenuSeparator},
                            nkadmin_util:menu_item(logout, menuEntry, #{icon=>'sign-out'}, State)
                        ]
                    }
                }
            ],
            State2 = nkadmin_util:add_object_tag(UserId, nkadmin_frame_user, State),
            {ok, Items, State2};
        {error, Error} ->
            {error, Error}
    end.

