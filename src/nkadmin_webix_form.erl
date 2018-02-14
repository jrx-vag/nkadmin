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
button(#{type:=disable}=Button, FormId) ->
    Disabled = maps:get(disabled, Button, false),
    IsDisabled = nkadmin_util:webix_bool(Disabled),
    #{
        id => <<"user_disable_button">>,
        view => <<"button">>,
        label => <<"Disable">>,
        type => <<"iconButton">>,
        icon => <<"ban">>,
        css => <<"webix_img_btn__centered">>,
        align => <<"center">>,
        %%hidden => (Enabled == <<"false">>),
        disabled => IsDisabled,
        click => #{
            nkParseFunction => js_fun_action(FormId, <<"disable">>)
        }
    };

button(#{type:=enable}=Button, FormId) ->
    Disabled = maps:get(disabled, Button, false),
    IsDisabled = nkadmin_util:webix_bool(Disabled),
    #{
        id => <<"user_enable_button">>,
        view => <<"button">>,
        label => <<"Enable">>,
        type => <<"iconButton">>,
        icon => <<"circle-thin">>,
        css => <<"webix_img_btn__centered">>,
        align => <<"center">>,
        %%hidden => (Enabled == <<"true">>),
        disabled => IsDisabled,
        click => #{
            nkParseFunction => js_fun_action(FormId, <<"enable">>)
        }
    };

button(#{type:=delete}=Button, FormId) ->
    Disabled = maps:get(disabled, Button, false),
    IsDisabled = nkadmin_util:webix_bool(Disabled),
    #{
        id => <<"user_delete_button">>,
        view => <<"button">>,
        label => <<"Delete">>,
        type => <<"iconButton">>,
        icon => <<"trash">>,
        css => <<"webix_img_btn__centered">>,
        align => <<"center">>,
        hidden => false,
        disabled => IsDisabled,
        click => #{
            nkParseFunction => js_fun_action(FormId, <<"delete">>)
        }
    };

button(#{type:=save}=Button, FormId) ->
    Disabled = maps:get(disabled, Button, false),
    IsDisabled = nkadmin_util:webix_bool(Disabled),
    #{
        id => <<"user_save_button">>,
        view => <<"button">>,
        label => <<"Save changes">>,
        type => <<"iconButton">>,
        icon => <<"floppy-o">>,
        css => <<"webix_img_btn__centered">>,
        align => <<"center">>,
        disabled => IsDisabled,
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
body_form(#{form_id:=FormId, groups:=Groups}=Spec, _Session) ->
    Base = case Spec of
        #{visible_batch := VisibleBatch} ->
            #{visibleBatch => VisibleBatch};
        _ ->
            #{}
    end,
    Base#{
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
body_form_group(#{header:=Header, values:=Values}=Group) ->
    Hidden = maps:get(hidden, Group, false),
    Disabled = maps:get(disabled, Group, false),
    Base = maps:with([id, batch], Group),
    Base#{
        minWidth => 300,
        rows => [
            #{
                template => Header,
                type => <<"section">>,
                hidden => nkadmin_util:webix_bool(Hidden),
                disabled => nkadmin_util:webix_bool(Disabled)
            } |
            [body_form_row(Row#{global_hidden => Hidden, global_disabled => Disabled}) || Row <- Values]
        ]
    }.


%% @private
body_form_row(#{type:=text, id:=Id, label:=Label, value:=Value}=Spec) ->
    Base = get_base_form_row(Spec),
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    Base2#{
        view => <<"text">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value
    };

%% @private
body_form_row(#{type:=number_edit, id:=Id, label:=Label, value:=Value}=Spec) ->
    Base = get_base_form_row(Spec),
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    IsFloat = maps:get(is_float, Spec, false),
    AllowNegatives = maps:get(allow_negatives, Spec, false),
    Base2#{
        view => <<"numberEdit">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        inputAlign => <<"left">>,
        value => Value,
        isFloat => IsFloat,
        allowNegatives => AllowNegatives
    };

body_form_row(#{type:=html, id:=Id, label:=Label, value:=Value}=Spec) ->
    Base = get_base_form_row(Spec),
    HiddenValue = maps:get(hidden, Base, false),
    Batch = maps:get(batch, Base, <<>>),
    #{
        batch => Batch,
        hidden => HiddenValue,
        cols => [
            #{
                view => <<"label">>,
                label => Label,
                width => 150,
                hidden => HiddenValue
            },
            #{
                view => <<"template">>,
                id => Id,
                template => Value, %nkdomain_admin_util:obj_id_url(CreatedBy),
                borderless => true,
                autoheight => true,
                hidden => HiddenValue
            }
        ]
    };

body_form_row(#{type:=password, id:=Id, label:=Label}=Spec) ->
    Base = get_base_form_row(Spec),
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    Base2#{
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

body_form_row(#{type:=combo, id:=Id, label:=Label, value:=Value, options:=Options}=Spec) ->
    Base = get_base_form_row(Spec),
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    Base2#{
        view => <<"combo">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        options => Options
    };

body_form_row(#{type:=suggest, id:=Id, label:=Label, value:=Value, options:=Options}=Spec) ->
    Base = get_base_form_row(Spec),
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    Base2#{
        view => <<"text">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        suggest => Options
    };

body_form_row(#{type:=multiselect, id:=Id, label:=Label, value:=Value, options:=Options}=Spec) ->
    Base = get_base_form_row(Spec),
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    Base2#{
        view => <<"multiselect">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        button => true,
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        options => Options
    };

body_form_row(#{type:=multicombo, id:=Id, label:=Label, value:=Value, options:=Options}=Spec) ->
    Base = get_base_form_row(Spec),
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    Base2#{
        view => <<"multicombo">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        options => Options
    };

body_form_row(#{type:=date, id:=Id, label:=Label, value:=Value}=Spec) ->
    Base = get_base_form_row(Spec),
    Base#{
        view => <<"text">>,
        %%id => Id,
        name => Id,
        label => Label,
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        disabled => false,
        readonly => true,
        on => #{
            onBeforeRender => #{
                nkParseFunction => js_fun_date()
            }
        }
    };

body_form_row(#{type:=checkbox, id:=Id, label:=Label, value:=Value}=Spec) ->
    Base = get_base_form_row(Spec),
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    Base2#{
        view => <<"checkbox">>,
        %%id => Id,
        name => Id,
        label => Label,
        labelWidth => 150,
        labelAlign => <<"left">>,
        checkValue => true,
        uncheckValue => false,
        required => false,
        value => nkadmin_util:webix_bool(Value)
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
        var form = $$(\"", FormId/binary, "\");
        if (form && form.validate()) {
            var values = form.getValues({disabled:true, hidden:true});
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
    }
">>.


%% @private
get_base_form_row(Spec) ->
    Hidden = maps:get(hidden, Spec, false),
    GlobalHidden = maps:get(global_hidden, Spec, false),
    HiddenValue = nkadmin_util:webix_bool(GlobalHidden or Hidden),
    Disabled = not maps:get(editable, Spec, false),
    GlobalDisabled = maps:get(global_disabled, Spec, false),
    DisabledValue = nkadmin_util:webix_bool(GlobalDisabled or Disabled),
    Required = maps:get(required, Spec, false),
    Batch = maps:get(batch, Spec, <<>>),
    Base = case Batch of
        <<>> ->
            #{};
        _ ->
            #{batch => Batch}
    end,
    Base#{
        required => Required,
        hidden => HiddenValue,
        disabled => DisabledValue
    }.


%% @private
get_form_listeners() ->
    [onChange].


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
