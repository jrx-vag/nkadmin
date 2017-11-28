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

-ifndef(NKADMIN_HRL_).
-define(NKADMIN_HRL_, 1).

%% ===================================================================
%% Defines
%% ===================================================================


-record(admin_session, {
    session_id :: nkdomain:obj_id(),
    domain_id :: nkdomain:obj_id(),
    base_path :: nkdomain:path(),
    srv_id :: nkservice:id(),
    user_id :: nkdomain:obj_id(),
    language :: binary(),
    url = <<>> :: binary(),
    detail = #{} :: map(),
    db_types = [] :: [nkdomain:type()], % Types currently used in db
    resources = [] :: [nkdomain:type()],
    sessions = [] :: #{nkdomain:type() => integer()},
    object_tags = #{} :: #{ObjId::nkdomain:obj_id() => [Tag::term]},
    key_data = #{} :: #{Key::binary() => term()}, % Map client keys to data
    special_urls = #{} :: #{Url::binary() => Key::binary()}, % Special urls like "/alerts"
    reg_pids = #{} :: #{pid()=>[domain|nkdomain:obj_id()]},
    http_auth_id :: binary()
}).


%-define(ADMIN_SESSION(Key, Val, Session), Session#{Key => Val}).

-define(DOMAIN_ADMIN_SESSION, <<"admin.session">>).



%% ===================================================================
%% Records
%% ===================================================================




-endif.

