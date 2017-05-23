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
-export([i18n/2, menu_item/3]).
-export([get_group/2, set_group/3, remove_group/2, get_element/3, get_element/4, add_element/4, remove_element/3]).

%% ===================================================================
%% Public
%% ===================================================================


%% @private
i18n(Key, Data) ->
    nklib_i18n:get(Key, maps:get(language, Data, <<"en">>)).


%% @doc
menu_item(Id, {menuCategory, Entries}, State) ->
    #{
        id => Id,
        class => menuCategory,
        value => #{
            label => nkadmin_util:i18n(Id, State),
            items => Entries
        }
    };

menu_item(Id, {menuGroup, Entries}, State) ->
    #{
        id => Id,
        class => menuGroup,
        value => #{
            label => nkadmin_util:i18n(Id, State),
            items => Entries
        }
    };

menu_item(Id, menuSimple, State) ->
    #{
        id => Id,
        class => menuSimple,
        value => #{label => nkadmin_util:i18n(Id, State)}
    };

menu_item(Id, {menuSimple, Label, Tip}, _State) ->
    #{
        id => Id,
        class => menuSimple,
        value => #{
            label => Label,
            tooltip => Tip
        }
    };

menu_item(Id, {menuBadge, Num}, State) ->
    #{
        id => Id,
        class => menuBadge,
        value => #{
            label => nkadmin_util:i18n(Id, State),
            badge => Num
        }
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


%% @private
to_bin(Term) when is_binary(Term)-> Term;
to_bin(Term) -> nklib_util:to_binary(Term).
