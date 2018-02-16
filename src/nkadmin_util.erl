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

-module(nkadmin_util).
-export([i18n/2, menu_category/3, menu_group/3, menu_entry/3, make_id/1]).
-export([get_key_data/2, set_key_data/3, remove_key_data/2]).
-export([update_detail/4, update_url/2]).
-export([get_special_url/2, set_special_url/3, remove_special_url/2]).
-export([get_object_tags/2, add_object_tag/3, remove_object_tag/3]).
-export([webix_bool/1, add_listeners/3]).

-include("nkadmin.hrl").

%% ===================================================================
%% Types
%% ===================================================================


%% ===================================================================
%% Updates management
%% ===================================================================


%% @doc
-spec menu_category(atom()|binary(), nkadmin:update_value(), nkadmin_session_obj:session()) ->
    map().

menu_category(Id, #{items:=_}=Value, Session) ->
    #{
        id => Id,
        class => menuCategory,
        value => add_label(Id, Value, Session)
    }.

%% @doc
-spec menu_group(atom()|binary(), nkadmin:update_value(), nkadmin_session_obj:session()) ->
    map().

menu_group(Id, #{items:=_}=Value, Session) ->
    #{
        id => Id,
        class => menuGroup,
        value => add_label(Id, Value, Session)
    }.


%% @doc
-spec menu_entry(atom()|binary(), nkadmin:update_value(), nkadmin_session_obj:session()) ->
    map().


menu_entry(Id, Value, Session) ->
    #{
        id => Id,
        class => menuEntry,
        value => add_label(Id, Value, Session)
    }.





%% @doc
update_detail(Path, Detail, Updates, Session) ->
    {Updates2, Session2} = update_path(Path, Updates, Session),
    Item = #{
        class => detail,
        id => detail,
        value => Detail
    },
    {[Item|Updates2], Session2}.


%% @doc
update_path(Path, Updates, #admin_session{base_path=_Base}=Session) ->
    Data = #{
        class => breadcrumbs,
        id => breadcrumbs,
        value => #{items => get_parts(Path)}
    },
    {[Data|Updates], Session#admin_session{url=Path}}.


%% @doc
update_url(Updates, #admin_session{url=Url}=Session) ->
%%    Path2 = case Base of
%%        <<"/">>  -> Path;
%%        _ -> <<Base/binary, Path/binary>>
%%    end,
    Data = #{
        class => url,
        id => url,
        value => #{label => Url}
    },
    {[Data|Updates], Session}.


%% @doc

make_id(List) ->
    nklib_util:bjoin(List, <<"__">>).


%% ===================================================================
%% Session management
%% ===================================================================


%% @doc Keys map client Ids to data
get_key_data(Key, #admin_session{key_data=Keys}) ->
    maps:get(to_bin(Key), Keys, undefined).


%% @doc
set_key_data(Key, Value, #admin_session{key_data=Keys}=Session) ->
    Keys2 = Keys#{to_bin(Key) => Value},
    Session#admin_session{key_data=Keys2}.


%% @doc
remove_key_data(Key, #admin_session{key_data=Keys}=Session) ->
    Keys2 = maps:remove(to_bin(Key), Keys),
    Session#admin_session{key_data=Keys2}.


%% @doc Maps Urls to Keys
get_special_url(Url, #admin_session{special_urls=Urls}) ->
    maps:get(to_bin(Url), Urls, undefined).


%% @doc
set_special_url(Url, Key, #admin_session{special_urls=Urls} = Session) ->
    Urls2 = Urls#{to_bin(Url) => Key},
    Session#admin_session{special_urls=Urls2}.


%% @doc
remove_special_url(Url, #admin_session{special_urls=Urls} = Session) ->
    Urls2 = maps:remove(to_bin(Url), Urls),
    Session#admin_session{special_urls=Urls2}.


%% @doc Associate a tag with an object
%% Not yet used, we will use it to subscribe to specific objects only
get_object_tags(ObjId, #admin_session{object_tags=Objects}) ->
    maps:get(ObjId, Objects, []).


%% @doc
add_object_tag(ObjId, Tag, #admin_session{object_tags=Objects}=Session) ->
    Tags = maps:get(ObjId, Objects, []),
    case lists:member(Tag, Tags) of
        true ->
            Session;
        false ->
            Objects2 = Objects#{ObjId => [Tag|Tags]},
            Session#admin_session{object_tags=Objects2}
    end.


%% @doc
remove_object_tag(ObjId, Tag, #admin_session{object_tags=Objects}=Session) ->
    Tags = maps:get(ObjId, Objects, []),
    case lists:member(Tag, Tags) of
        true ->
            Objects2 = case Tags -- [Tag] of
                [] ->
                    maps:remove(ObjId, Objects);
                Tags2 ->
                    Objects#{ObjId => Tags2}
            end,
            Session#admin_session{object_tags=Objects2};
        false ->
            Session
    end.


%% ===================================================================
%% Util
%% ===================================================================


%% @private
i18n(Key, #admin_session{srv_id=SrvId, language=Lang}) ->
    case apply(SrvId, i18n, [SrvId, Key, Lang]) of
        <<>> ->
            lager:warning("Missing i18n: ~s", [Key]),
            <<>>;
        Other ->
            Other
    end.

%% @doc
get_parts(Path) ->
    case binary:split(to_bin(Path), <<"/">>, [global]) of
        [<<>>, <<>>] -> [<<"/">>];
        [<<>>|Rest] -> [<<"/">>|Rest]
    end.


%% ===================================================================
%% Util
%% ===================================================================


%% @private
webix_bool(<<"false">>) ->
    false;

webix_bool(<<"true">>) ->
    true;

webix_bool(true) ->
    true;

webix_bool(false) ->
    false;

webix_bool(Unknown) ->
    lager:warning("nkadmin_util: unknown boolean value passed to webix_bool: ~p", [Unknown]),
    false.


%% @private
add_listeners([], _Spec, Map) ->
    Map;

add_listeners(Listeners, Spec, Map) ->
    add_listeners(Listeners, #{}, Spec, Map).


add_listeners([], Listeners, _Spec, Map) ->
    case maps:size(Listeners) of
        0 ->
            Map;
        _ ->
            Map#{
                on => Listeners
            }
    end;

add_listeners([L|Ls], Listeners, Spec, Map) ->
    case Spec of
        #{L := F} ->
            Listeners2 = Listeners#{
                L => #{
                    nkParseFunction => F
                }
            },
            add_listeners(Ls, Listeners2, Spec, Map);
        _ ->
            add_listeners(Ls, Listeners, Spec, Map)
    end.





%% ===================================================================
%% Internal
%% ===================================================================

%% @private
add_label(Id, Value, Session) ->
    case maps:is_key(label, Value) of
        true -> Value;
        false -> Value#{label=>nkadmin_util:i18n(Id, Session)}
    end.


%%%% @doc
%%append_type(<<"/">>, Type) -> <<$/, (to_bin(Type))/binary>>;
%%append_type(Path, Type) -> <<(to_bin(Path))/binary, $/, (to_bin(Type))/binary>>.


%% @private
to_bin(Term) when is_binary(Term)-> Term;
to_bin(Term) -> nklib_util:to_binary(Term).
