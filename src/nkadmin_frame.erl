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
get_frame(Session) ->
    case frame_domain(Session) of
        {ok, Items1, Session2} ->
            case frame_user(Session2) of
                {ok, Items2, Session3} ->
                    Value = #{
                        class => frame,
                        value => #{items => Items1 ++ Items2}
                    },
                    {ok, Value, Session3};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error, Session}
    end.


%% @doc
event(#nkevent{type = <<"updated">>, obj_id=ObjId}, Updates, Session) ->
    case Session of
        #{domain_id:=ObjId} ->
            {ok, Items, Session2} = frame_domain(Session),
            {ok, Items++Updates, Session2};
        #{user_id:=ObjId} ->
            {ok, Items, Session2} = frame_user(Session),
            {ok, Items++Updates, Session2};
        _ ->
            {ok, Updates, Session}
    end;

event(#nkevent{type = <<"deleted">>, obj_id=ObjId}, Updates, Session) ->
    case Session of
        #{domain_id:=ObjId} ->
            nkdomain:unload(any, self(), domain_deleted),
            {ok, Updates, Session};
        #{user_id:=ObjId} ->
            nkdomain:unload(any, self(), user_deleted),
            {ok, Updates, Session};
        _ ->
            {ok, Updates, Session}
    end;

event(_Event, Updates, Session) ->
    {ok, Updates, Session}.


%% @doc
element_action(_ElementId, _Id, _Value, Updates, Session) ->
    {ok, Updates, Session}.


%% ===================================================================
%% Internal
%% ===================================================================



%% @private
frame_domain(#{srv_id:=SrvId, domain_id:=DomainId}=Session) ->
    case nkdomain:get_name(SrvId, DomainId) of
        {ok, #{name:=DomName}} ->
            Items = [
                #{
                    id => admin_frame_domain_name,
                    class => frameDomainName,
                    value => #{label => DomName, css => DomName}
                },
                #{
                    id => admin_frame_domain_icon,
                    class => frameDomainIcon,
                    value => #{icon => <<>>}
                }
            ],
            Session2 = nkadmin_util:add_object_tag(DomainId, nkadmin_frame_domain, Session),
            {ok, Items, Session2};
        {error, Error} ->
            {error, Error}
    end.


%% @private
frame_user(#{srv_id:=SrvId, user_id:=UserId}=Session) ->
    case nkdomain_user_obj:get_name(SrvId, UserId) of
        {ok, #{<<"user">>:=#{name:=UserName, surname:=UserSurname}}=Obj} ->
            UserIconId = maps:get(icon_id, Obj, <<>>),
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
                            nkadmin_util:menu_item(admin_frame_user_menu_account, menuEntry, #{icon=>gear}, Session),
                            #{class => frameUserMenuSeparator},
                            nkadmin_util:menu_item(admin_frame_user_menu_messages, menuEntry, #{icon=>comments}, Session),
                            #{class => frameUserMenuSeparator},
                            nkadmin_util:menu_item(logout, menuEntry, #{icon=>'sign-out'}, Session)
                        ]
                    }
                }
            ],
            Session2 = nkadmin_util:add_object_tag(UserId, nkadmin_frame_user, Session),
            {ok, Items, Session2};
        {error, Error} ->
            {error, Error}
    end.

