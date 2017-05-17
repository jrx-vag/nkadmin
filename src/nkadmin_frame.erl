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
-export([get_frame/1]).


%%-include_lib("nkservice/include/nkservice.hrl").
%%-include_lib("nkdomain/include/nkdomain.hrl").
%%-include_lib("nkapi/include/nkapi.hrl").
%%-include("nkadmin.hrl").
%%
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
                    case frame_user_menu(State) of
                        {ok, Frame3} ->
                            {ok, Frame1 ++ Frame2 ++ Frame3};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%% @private
frame_domain( #{srv_id:=SrvId, domain_id:=DomainId}) ->
    case nkdomain:get_name(SrvId, DomainId) of
        {ok, #{name:=DomName, icon_id:=DomIconId}} ->
            {ok, [
                #{
                    id => frame_domain_name,
                    class => frameDomainName,
                    value => DomName
                },
                #{
                    id => frame_domain_icon,
                    class => frameDomainIcon,
                    value => DomIconId
                }
            ]};
        {error, Error} ->
            {error, Error}
    end;

frame_domain(_) ->
    {ok, []}.


%% @private
frame_user(#{srv_id:=SrvId, user_id:=UserId}) ->
    case nkdomain_user_obj:get_name(SrvId, UserId) of
        {ok, #{<<"user">>:=#{name:=UserName, surname:=UserSurname, icon_id:=UserIconId}}} ->
            {ok, [
                #{
                    id => frame_user_name,
                    class => userName,
                    value => <<UserName/binary, " ", UserSurname/binary>>
                },
                #{
                    id => frame_user_icon,
                    class => frameUserIcon,
                    value => UserIconId
                }
            ]};
        {error, Error} ->
            {error, Error}
    end;

frame_user(_) ->
    {ok, []}.


%% @private
frame_user_menu(#{user_menu:=true}=Data) ->
    {ok, [
        #{
            id => frame_user_menu,
            class => frameUserMenu,
            value => #{
                items => [
                    #{
                        id => frame_user_menu_account,
                        class => frameUserMenuItem,
                        value => i18n(frame_user_menu_account, Data)
                    },
                    #{
                        id => frame_user_menu_messages,
                        class => frameUserMenuItem,
                        value => i18n(frame_user_menu_messages, Data)
                    },
                    #{
                        class => frameUserMenuSeparator
                    }
                ]
            }
        }
    ]};

frame_user_menu(_) ->
    {ok, []}.


%% @private
i18n(Key, Data) ->
    nkadmin_util:i18n(Key, Data).

