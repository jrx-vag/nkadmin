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

-module(nkadmin_syntax).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([syntax/5, get_info/1]).


%% ===================================================================
%% Syntax
%% ===================================================================

syntax('', create, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            domain => binary
        },
        Defaults,
        [domain|Mandatory]
    };

syntax('', switch_domain, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            admin_session_id => binary,
            domain => binary
        },
        Defaults,
        [admin_session_id, domain|Mandatory]
    };

syntax('', switch_object, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            admin_session_id => binary,
            obj_id => binary
        },
        Defaults,
        [admin_session_id, obj_id|Mandatory]
    };

syntax('', destroy, Syntax, Defaults, Mandatory) ->
    {
        Syntax#{
            admin_session_id => binary
        },
        Defaults,
        [admin_session_id|Mandatory]
    };

syntax(_Sub, _Cmd, Syntax, Defaults, Mandatory) ->
    {Syntax, Defaults, Mandatory}.



%% ===================================================================
%% Internal
%% ===================================================================


-spec get_info(nkadmin:admin()) ->
     nkadmin:admin().

get_info(Admin) ->
    Fields = [obj_id, frame, tree, detail],
    maps:with([Fields], Admin).


