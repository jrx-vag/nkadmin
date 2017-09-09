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

-module(nkadmin).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export_type([update/0]).

%% ===================================================================
%% Types
%% ===================================================================


-type update() ::
    #{
        class => update_class(),        % Mandatory
        id => update_id(),              % Optional
        value => update_value()
    }.

-type update_id() :: binary().

-type update_class() ::
    frame |
    tree |
    detail |
    url |
    breadcrumbs |
    frameDomainName | frameDomainIcon |
    frameUserIcon | frameUserIcon | frameUserMenu |
    menuCategory | menuGroup | menuEntry.



-type update_value() ::
    #{
        label => binary(),
        css => binary(),
        icon => binary(),
        tooltip => binary(),
        items => [update()] | [binary()]
    }.



%% ===================================================================
%% Public functions
%% ===================================================================
