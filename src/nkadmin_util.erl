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
-export([i18n/2, menu_item/4, get_parts/1, append_type/2]).
-export([get_key/2, add_key/3, remove_key/2]).
-export([append_path/3, update_path/2]).
-export([get_object_tags/2, add_object_tag/3, remove_object_tag/3]).


%% ===================================================================
%% Types
%% ===================================================================

-type menu_type() ::  menuCategory | menuGroup | menuEntry.

-type menu_value() :: #{
    label => binary(),
    icon => atom() | binary(),
    tooltip => binary(),
    items => list()
}.


%% ===================================================================
%% Public
%% ===================================================================


%% @private
i18n(Key, Data) ->
    nklib_i18n:get(Key, maps:get(language, Data, <<"en">>)).


%% @doc
-spec menu_item(atom()|binary(), menu_type(), menu_value(), nkadmin_session_obj:session()) ->
    map().

menu_item(Id, menuCategory, #{items:=_}=Value, Session) ->
    #{
        id => Id,
        class => menuCategory,
        value => add_label(Id, Value, Session)
    };

menu_item(Id, menuGroup, #{items:=_}=Value, Session) ->
    #{
        id => Id,
        class => menuGroup,
        value => add_label(Id, Value, Session)
    };

menu_item(Id, menuEntry, Value, Session) ->
    #{
        id => Id,
        class => menuEntry,
        value => add_label(Id, Value, Session)
    }.


%% @doc
get_key(Key, #{keys:=Keys}) ->
    maps:get(to_bin(Key), Keys, undefined).


%% @doc
add_key(Key, Value, #{keys:=Keys}=Session) ->
    Keys2 = Keys#{to_bin(Key) => Value},
    Session#{keys:=Keys2}.


%% @doc
remove_key(Key, #{keys:=Keys}=Session) ->
    Keys2 = maps:remove(to_bin(Key), Keys),
    Session#{keys:=Keys2}.



%% @doc
get_object_tags(ObjId, #{objects:=Objects}) ->
    maps:get(ObjId, Objects, []).


%% @doc
add_object_tag(ObjId, Tag, #{objects:=Objects}=Session) ->
    Tags = maps:get(ObjId, Objects, []),
    case lists:member(Tag, Tags) of
        true ->
            Session;
        false ->
            Objects2 = Objects#{ObjId => [Tag|Tags]},
            Session#{objects:=Objects2}
    end.


%% @doc
remove_object_tag(ObjId, Tag, #{objects:=Objects}=Session) ->
    Tags = maps:get(ObjId, Objects, []),
    case lists:member(Tag, Tags) of
        true ->
            Objects2 = case Tags -- [Tag] of
                [] ->
                    maps:remove(ObjId, Objects);
                Tags2 ->
                    Objects#{ObjId => Tags2}
            end,
            Session#{objects:=Objects2};
        false ->
            Session
    end.




%% @doc
get_parts(Path) ->
    case binary:split(to_bin(Path), <<"/">>, [global]) of
        [<<>>, <<>>] -> [<<"/">>];
        [<<>>|Rest] -> [<<"/">>|Rest]
    end.


%% @doc
append_type(<<"/">>, Type) -> <<$/, (to_bin(Type))/binary>>;
append_type(Path, Type) -> <<(to_bin(Path))/binary, $/, (to_bin(Type))/binary>>.



%% @private
append_path(Path, Updates, #{domain_path:=Base}) ->
    update_path(append_type(Base, Path), Updates).


%% @private
update_path(Path, Updates) ->
    [
        #{
            class => url,
            id => url,
            value => #{label => Path}
        },
        #{
            class => breadcrumbs,
            id => breadcrumbs,
            value => #{items => nkadmin_util:get_parts(Path)}
        },
        #{
            class => detail,
            value => #{}
        }
        | Updates
    ].




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
add_label(Id, Value, Session) ->
    case maps:is_key(label, Value) of
        true -> Value;
        false -> Value#{label=>nkadmin_util:i18n(Id, Session)}
    end.


%% @private
to_bin(Term) when is_binary(Term)-> Term;
to_bin(Term) -> nklib_util:to_binary(Term).
