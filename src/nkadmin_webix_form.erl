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

-module(nkadmin_webix_form).
-export([form/3]).

-include("nkadmin.hrl").
-include_lib("nkadmin/include/nkadmin.hrl").

%% ===================================================================
%% Types
%% ===================================================================


-type spec() ::
    #{
        form_id => binary(),           %% Mandatory
        is_enabled => boolean(),
        with_image => binary(),
        groups => [
            #{
                header => binary(),
                values => [
                    #{
                        id => binary(),
                        type => text | password | html | date | combo,
                        label => binary(),
                        value => binary() | integer(),
                        editable => boolean(),
                        options => [#{id:=binary(), value:=binary()}]
                    }
                ]
            }
        ]
    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec form(spec(), nkdomain:type(), #admin_session{}) ->
    map().

form(Spec, Type, Session) ->
    UpType = nklib_util:to_upper(Type),
    #{
        id => <<"body">>,
        view => <<"accordion">>,
        type => <<"clean">>,
        height => <<"100%">>,
        margin => 0,
        borderless => false,
        multi => <<"mixed">>,
        rows => [
            buttons(Spec),
            #{
                header => <<UpType/binary, " DETAILS">>,
                body => body(Spec, Session)
            },
            #{
                view => <<"resizer">>
            },
            %%            #{
            %%                header => <<"SUBTABLES">>,
            %%                body => subtables(FormId, Obj)
            %%            },
            #{
                gravity => 0.0
            }
        ]
    }.


%% @private
buttons(#{buttons:=Buttons, form_id:=FormId}) ->
    #{
        id => <<"user_buttons">>,
        type => <<"space">>,
        view => <<"layout">>,
        cols => lists:flatten([button(Button, FormId) || Button <- Buttons])
    }.


%% @private
button(#{type:=disable}, FormId) ->
    #{
        id => <<"user_disable_button">>,
        view => <<"button">>,
        label => <<"Disable">>,
        type => <<"iconButton">>,
        icon => <<"ban">>,
        css => <<"webix_img_btn__centered">>,
        align => <<"center">>,
        %%hidden => (Enabled == <<"false">>),
        click => #{
            nkParseFunction => js_fun_action(FormId, <<"disable">>)
        }
    };

button(#{type:=enable}, FormId) ->
    #{
        id => <<"user_enable_button">>,
        view => <<"button">>,
        label => <<"Enable">>,
        type => <<"iconButton">>,
        icon => <<"circle-thin">>,
        css => <<"webix_img_btn__centered">>,
        align => <<"center">>,
        %%hidden => (Enabled == <<"true">>),
        click => #{
            nkParseFunction => js_fun_action(FormId, <<"enable">>)
        }
    };

button(#{type:=delete}, FormId) ->
    #{
        id => <<"user_delete_button">>,
        view => <<"button">>,
        label => <<"Delete">>,
        type => <<"iconButton">>,
        icon => <<"trash">>,
        css => <<"webix_img_btn__centered">>,
        align => <<"center">>,
        hidden => false,
        click => #{
            nkParseFunction => js_fun_action(FormId, <<"delete">>)
        }
    };

button(#{type:=save}, FormId) ->
    #{
        id => <<"user_save_button">>,
        view => <<"button">>,
        label => <<"Save changes">>,
        type => <<"iconButton">>,
        icon => <<"floppy-o">>,
        css => <<"webix_img_btn__centered">>,
        align => <<"center">>,
        disabled => false,
        hidden => false,
        click => #{
            nkParseFunction => js_fun_action_form(FormId, <<"save">>)
        }
    }.



%% @private
body(Spec, Session) ->
    #{
        id => <<"form_body">>,
        %% header => <<"USER DETAILS">>,
        type => <<"clean">>,
        gravity => 2.0,
        height => <<"100%">>,
        margin => 0,
        borderless => false,
        cols => case Spec of
            #{with_image:=Image} ->
                [
                    #{
                        view => <<"layout">>,
                        height => <<"100%">>,
                        rows => [
                            #{
                                height => 30
                            },
                            #{
                                view => <<"label">>,
                                label => Image,
                                width => 200,
                                height => 150,
                                align => <<"center">>
                            },
                            #{
                                % SPACER
                            }
                        ]
                    },
                    body_form(Spec, Session)
                ];
            _ ->
                [
                    body_form(Spec, Session)
                ]
        end
    }.


%% @private
body_form(#{form_id:=FormId, groups:=Groups}, _Session) ->
    #{
        id => FormId,
        view => <<"form">>,
        height => <<"100%">>,
        scroll => <<"y">>,
        margin => 0,
        elements => [body_form_group(Group) || Group <- Groups],
        elementsConfig => #{
            labelAlign => <<"right">>
        }
    }.


%% @private
body_form_group(#{header:=Header, values:=Values}) ->
    #{
        minWidth => 300,
        rows => [
            #{
                template => Header,
                type => <<"section">>,
                hidden => false
            } |
            [body_form_row(Row) || Row <- Values]
        ]
    }.


%% @private
body_form_row(#{type:=text, id:=Id, label:=Label, value:=Value}=Spec) ->
    #{
        view => <<"text">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        disabled => not maps:get(editable, Spec, false)
    };

body_form_row(#{type:=html, id:=Id, label:=Label, value:=Value}) ->
    #{
        cols => [
            #{
                view => <<"label">>,
                label => Label,
                width => 150,
                disabled => true
            },
            #{
                view => <<"template">>,
                id => Id,
                template => Value, %nkdomain_admin_util:obj_id_url(CreatedBy),
                borderless => true,
                autoheight => true
            }
        ]
    };

body_form_row(#{type:=password, id:=Id, label:=Label}) ->
    #{
        view => <<"text">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => <<"new ", (placeholder(Label))/binary>>,
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => <<"">>,
        type => <<"password">>
    };

body_form_row(#{type:=combo, id:=Id, label:=Label, value:=Value, options:=Options}) ->
    #{
        view => <<"combo">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        disabled => false, % true/false
        hidden => false, % true/false
        options => Options
    };

body_form_row(#{type:=date, label:=Label, value:=Value}) ->
    #{
        view => <<"text">>,
        label => Label,
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        on => #{
            onBeforeRender => #{
                nkParseFunction => js_fun_date()
            }
        },
        disabled => true
    }.


%% @private
placeholder(Value) ->
    <<(nklib_util:to_lower(Value))/binary, "...">>.




%% @private
js_fun_date() -> <<"
    function(data) {   // 'en-US', 'es-ES', etc.
        if (data && data.value && typeof data.value === 'number') {
            data.value = (new Date(data.value)).toLocaleString();
        }
    }
">>.


%% @private
js_fun_action(FormId, Action) -> <<"
    function() {
        ncClient.sendMessageAsync(
            \"objects/admin.session/element_action\",
            {
                element_id: \"", FormId/binary, "\",
                action: \"", Action/binary, "\"
            }).then(
                function(response) {
                    console.log('Action ", Action/binary, " button clicked OK: ', response);
                    if (response.data && response.data.elements) {
                        updateView(response.data.elements);
                    }
                }).catch(
                    function(response) {
                        console.log('Action ", Action/binary, " error: ', response);
                        webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error
                });
        });
    }
">>.


%% @private
js_fun_action_form(FormId, Action) -> <<"
    function() {
        var values = $$(\"", FormId/binary, "\").getValues({disabled:false});
        ncClient.sendMessageAsync(
            \"objects/admin.session/element_action\",
            {
                element_id: \"", FormId/binary, "\",
                action: \"", Action/binary, "\",
                value: values
            }).then(
                function(response) {
                    console.log('Action ", Action/binary, " button clicked OK: ', response);
                    if (response.data && response.data.elements) {
                        updateView(response.data.elements);
                    }
                }).catch(
                    function(response) {
                        console.log('Action ", Action/binary, " error: ', response);
                        webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error
                });
        });
    }
">>.



%%function() {
%%//var values = $$(\"", FormId/binary, "\").getValues({disabled:false});
%%        ncClient.sendMessageAsync(
%%            \"objects/admin.session/element_action\",
%%            {
%%                element_id: \"", FormId/binary, "\",
%%                action: \"save\",
%%                value: \"values\"
%%            }).then(
%%                function(response) {
%%                    console.log('Form action ", Action/binary, " button clicked OK: ', response);
%%                    if (response.data && response.data.elements) {
%%                        // Update view
%%                        updateView(response.data.elements);
%%                    }
%%                }).catch(
%%                    function(response) {
%%                        console.log('Error at save button clicked: ', response);
%%                        webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error });
%%                });
%%        });
%%    }

%%i18n(<<"&nbsp;">>, _Session) -> <<>>;
%%i18n(Key, Session) -> nkadmin_util:i18n(Key, Session).
%%
%%
%%%% @private
%%to_bin(Term) when is_binary(Term) -> Term;
%%to_bin(Term) -> nklib_util:to_binary(Term).
