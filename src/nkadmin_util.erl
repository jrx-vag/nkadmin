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
-export([get_group/2, set_group/3, remove_group/2, get_element/3, get_element/4, add_element/4, remove_element/3]).


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
-spec menu_item(atom()|binary(), menu_type(), menu_value(), nkadmin_session_obj:state()) ->
    map().

menu_item(Id, menuCategory, #{items:=_}=Value, State) ->
    #{
        id => Id,
        class => menuCategory,
        value => add_label(Id, Value, State)
    };

menu_item(Id, menuGroup, #{items:=_}=Value, State) ->
    #{
        id => Id,
        class => menuGroup,
        value => add_label(Id, Value, State)
    };

menu_item(Id, menuEntry, Value, State) ->
    #{
        id => Id,
        class => menuEntry,
        value => add_label(Id, Value, State)
    }.


%% @doc
get_group(Id, #{elements:=Elements}) ->
    maps:get(to_bin(Id), Elements, #{}).


%% @doc
set_group(Id, Data, #{elements:=Elements}=State) ->
    Elements2 = Elements#{to_bin(Id) => Data},
    State#{elements:=Elements2}.


%% @doc
remove_group(Id, #{elements:=Elements}=State) ->
    Elements2 = maps:remove(to_bin(Id), Elements),
    State#{elements:=Elements2}.


%% @doc
get_element(GroupId, Id, State) ->
    get_element(GroupId, Id, undefined, State).


%% @doc
get_element(GroupId, Id, Default, State) ->
    Group = get_group(GroupId, State),
    maps:get(to_bin(Id), Group, Default).



%% @doc
add_element(GroupId, Id, Value, #{elements:=Elements}=State) ->
    GroupId2 = to_bin(GroupId),
    Group1 = maps:get(GroupId2, Elements, #{}),
    Group2 = Group1#{to_bin(Id) => Value},
    Elements2 = Elements#{GroupId2 => Group2},
    State#{elements:=Elements2}.


%% @doc
remove_element(GroupId, Id, #{elements:=Elements}=State) ->
    GroupId2 = to_bin(GroupId),
    Group1 = maps:get(GroupId2, Elements, #{}),
    Group2 = maps:remove(to_bin(Id), Group1),
    Elements2 = case map_size(Group2) of
        0 -> maps:remove(GroupId2, Elements);
        _ -> Elements#{GroupId2 => Group2}
    end,
    State#{elements:=Elements2}.


%% @doc
get_parts(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [<<>>, <<>>] -> [<<"/">>];
        [<<>>|Rest] -> [<<"/">>|Rest]
    end.


%% @doc
append_type(<<"/">>, Type) -> <<$/, (to_bin(Type))/binary>>;
append_type(Path, Type) -> <<Path/binary, $/, (to_bin(Type))/binary>>.



%% ===================================================================
%% Internal
%% ===================================================================

%% @private
add_label(Id, Value, State) ->
    case maps:is_key(label, Value) of
        true -> Value;
        false -> Value#{label=>nkadmin_util:i18n(Id, State)}
    end.


%% @private
to_bin(Term) when is_binary(Term)-> Term;
to_bin(Term) -> nklib_util:to_binary(Term).
