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
-export([i18n/2, menu_item/4]).
-export([get_key_data/2, set_key_data/3, remove_key_data/2]).
-export([update_detail/4]).
-export([get_url_key/2, set_url_key/3, remove_url_key/2]).
-export([get_object_tags/2, add_object_tag/3, remove_object_tag/3]).

-include("nkadmin.hrl").

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
%% Updates management
%% ===================================================================



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
update_detail(Path, Detail, Updates, Session) ->
    {Updates2, Session2} = update_path(Path, Updates, Session),
    Item = #{
        class => detail,
        id => detail,
        value => Detail
    },
    {[Item|Updates2], Session2}.


%% @doc
update_path(Path, Updates, #admin_session{domain_path=Base}=Session) ->
    Path2 = case Base of
        <<"/">>  -> Path;
        _ -> <<Base/binary, Path/binary>>
    end,
    Updates2 = [
          #{
              class => url,
              id => url,
              value => #{label => Path2}
          },
          #{
              class => breadcrumbs,
              id => breadcrumbs,
              value => #{items => get_parts(Path2)}
          }
        | Updates
    ],
    {Updates2, Session#admin_session{detail_url=Path}}.




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
get_url_key(Url, #admin_session{url_to_key=Urls}) ->
    maps:get(to_bin(Url), Urls, undefined).


%% @doc
set_url_key(Url, Key, #admin_session{url_to_key=Urls}=Session) ->
    Urls2 = Urls#{to_bin(Url) => Key},
    Session#admin_session{url_to_key=Urls2}.


%% @doc
remove_url_key(Url, #admin_session{url_to_key=Urls}=Session) ->
    Urls2 = maps:remove(to_bin(Url), Urls),
    Session#admin_session{url_to_key=Urls2}.


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
i18n(Key, #admin_session{language=Lang}) ->
    case nklib_i18n:get(Key, Lang) of
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
