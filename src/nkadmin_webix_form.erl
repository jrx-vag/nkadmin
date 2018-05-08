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
-export([form/3, creation_fields/2]).
-export([create_upload_onclick/1, create_upload_onclick/2]).
-export([js_fun_icon_upload_form/2]).

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
        with_css => binary(),
        with_file_types => [term()],
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
        type => <<"space">>,
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


%% @doc
-spec creation_fields(nkdomain:obj(), boolean()) ->
    map().

creation_fields(Obj, IsNew) ->
    CreatedBy = maps:get(created_by, Obj, <<>>),
    CreatedTime = maps:get(created_time, Obj, 0),
    UpdatedBy = maps:get(updated_by, Obj, <<>>),
    UpdatedTime = maps:get(updated_time, Obj, 0),
    #{
        header => <<"CREATION DATA">>,
        hidden => IsNew,
        values => [
            #{
                id => <<"created_by">>,
                type => html,
                label => <<"Created by">>,
                value => nkdomain_admin_util:obj_id_url(CreatedBy),
                hidden => CreatedBy =:= <<>>,
                editable => false
            },
            #{
                id => <<"created_time">>,
                type => date,
                label => <<"Created time">>,
                value => CreatedTime,
                hidden => CreatedBy =:= <<>>,
                editable => false
            },
            #{
                id => <<"updated_by">>,
                type => html,
                label => <<"Updated by">>,
                value => nkdomain_admin_util:obj_id_url(UpdatedBy),
                hidden => UpdatedBy =:= <<>>,
                editable => false
            },
            #{
                id => <<"updated_time">>,
                type => date,
                label => <<"Updated time">>,
                value => UpdatedTime,
                hidden => UpdatedBy =:= <<>>,
                editable => false
            }
        ]
    }.


%% @doc
-spec create_upload_onclick(map()) ->
    map().

create_upload_onclick(Opts) ->
    create_upload_onclick(<<>>, Opts).


%% @doc
-spec create_upload_onclick(binary(), map()) ->
    map().

create_upload_onclick(FormId, Opts) ->
    Types = maps:get(types, Opts, []),
    OnSuccess = maps:get(on_success, Opts, <<>>),
    OnFailure = maps:get(on_failure, Opts, <<>>),
    OnProgress = maps:get(on_progress, Opts, <<>>),
    Domain = maps:get(domain, Opts, <<>>),
    StoreId = maps:get(store_id, Opts, <<>>),
    DomainField = maps:get(domain_field, Opts, <<>>),
    StoreIdField = maps:get(store_id_field, Opts, <<>>),
    Suc = get_js_function(OnSuccess),
    Fail = get_js_function(OnFailure, <<"function(error, xhr) {
        console.log('ERROR: while uploading file', error, xhr);
        webix.message({'type': 'error', 'text': 'ERROR while uploading file'});
    }">>),
    Prog = get_js_function(OnProgress), <<"function(item, progress) {
        console.log('Upload progress: ', item, progress);
    }">>,
    Types2 = [<<"'", (nklib_util:to_binary(Type))/binary, "'">> || Type <- Types],
    Types3 = list_to_binary(lists:join(<<",">>, Types2)),
    ValidateForm = case FormId of
        <<>> ->
            <<>>;
        _ ->
            <<" 
                var form = $$(\"", FormId/binary, "\");
                if (form && !form.validate()) {
                    return;
                }
            ">>
    end,
    <<"function() {
        var api = $$('uploadAPI');
        var id = null;
        if (this.config && this.config.id) {
            id = this.config.id;
        }
        if (api) {
            // Check if form is valid
            ", ValidateForm/binary, "
            api.fileDialog({
                id: id,
                types: [", Types3/binary, "],
                progress: {
                    type: 'icon'
                },
                form: '", FormId/binary, "',
                domain: '", Domain/binary, "',
                store_id: '", StoreId/binary, "',
                domain_field: '", DomainField/binary, "',
                store_id_field: '", StoreIdField/binary, "',
                callback: {
                    success: ", Suc/binary, ",
                    error: ", Fail/binary, ",
                    updateProgress: ", Prog/binary, "
                }
            });
        } else {
            console.log('ERROR: (' + id + ' onClick) uploadAPI not found');
        }
    }">>.




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
body(#{form_id:=FormId}=Spec, Session) ->
    %OnClick = maps:get(with_on_click, Spec, <<"function(){}">>),
    FileTypes = maps:get(with_file_types, Spec, ["png", "jpg", "gif", "jpeg"]),
    OnClick = js_fun_icon_upload_form(FormId, #{types => FileTypes}),
    CSS = maps:get(with_css, Spec, <<>>),
    CSSOnClick = case CSS of
        <<>> ->
            #{};
        _ ->
            #{
                CSS => #{
                    <<"nkParseFunction">> => OnClick
                }
            }
    end,
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
                                view => <<"template">>,
                                borderless => true,
                                template => Image,
                                width => 200,
                                height => <<"auto">>,
                                align => <<"center">>,
                                onClick => CSSOnClick
                            }
                        ]
                    },
                    #{
                        view => <<"resizer">>
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
    HeaderBase = maps:with([batch], Group),
    Base#{
        minWidth => 300,
        rows => [
            HeaderBase#{
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
    Editable = maps:get(editable, Spec, true),
    Base2#{
        view => <<"text">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        readonly => not nkadmin_util:webix_bool(Editable),
        disabled => false
    };

%% @private
body_form_row(#{type:=textarea, id:=Id, label:=Label, value:=Value}=Spec) ->
    Base = get_base_form_row(Spec),
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    Editable = maps:get(editable, Spec, true),
    Height = maps:get(height, Spec, 200),
    Base2#{
        view => <<"textarea">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        height => Height,
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        readonly => not nkadmin_util:webix_bool(Editable),
        disabled => false
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
        css => <<"nk_html_link">>,
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

body_form_row(#{type:=View, id:=Id, label:=Label, value:=Value, suggest_type:=Type, suggest_field:=Field}=Spec)
when View =:= combo orelse View =:= suggest orelse View =:= multicombo ->
    Options = maps:get(options, Spec, []),
    Filters = maps:get(suggest_filters, Spec, #{}),
    FiltersJSON = nklib_json:encode_pretty(Filters),
    Sort = maps:get(suggest_sort, Spec, #{}),
    TimeOut = maps:get(suggest_key_press_timeout, Spec, ?DEFAULT_ADMIN_KEY_PRESS_TIMEOUT),
    Start = maps:get(suggest_start, Spec, 0),
    End = maps:get(suggest_end, Spec, ?DEFAULT_ADMIN_SUGGEST_MAX_SIZE),
    StartBin = nklib_util:to_binary(Start),
    EndBin = nklib_util:to_binary(End),
    Template = maps:get(suggest_template, Spec, <<>>),
    SuggestBody = case Template of
        <<>> ->
            #{};
        _ ->
            #{
                template => Template
            }
    end,
    View2 = case View of
        combo ->
            <<"combo">>;
        multicombo ->
            <<"multicombo">>;
        suggest ->
            <<"text">>
    end,
    Base = get_base_form_row(Spec),
    #{readonly := ReadOnly} = Base,
    Base2 = nkadmin_util:add_listeners(get_form_listeners(), Spec, Base),
    Base2#{
        view => View2,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value,
        suggest => #{
            keyPressTimeout => TimeOut,
            data => Options,
            body => SuggestBody#{
                nkOpts => #{
                    id => nkdomain_admin_util:make_type_view_id(Type, <<"/">>),
                    field => Field,
                    sort => Sort
                },
                dataFeed => #{
                    <<"nkParseFunction">> => <<"
                        function(text) {
                            suggest = this;
                            filters = ", FiltersJSON/binary, ";
                            filters['", Field/binary, "'] = text;
                            opts = {
                                filter: filters,
                                sort: this.config.nkOpts.sort,
                                start: ", StartBin/binary, ",
                                end: ", EndBin/binary, ",
                                readonly: ", (nklib_util:to_binary(ReadOnly))/binary, "
                            };
                            if (!opts.readonly) {
                                get_data(this.config.nkOpts.id, opts)
                                .then(function(response) {
                                    if (suggest) {
                                        suggest.clearAll();
                                        suggest.parse(response.data.data);
                                    }
                                }).catch(function(response) {
                                    console.log('get_data: GOT ERROR: ', response);
                                });
                            }
                        }
                    ">>
                }
            }
        },
        on => #{
            onItemClick => #{
                <<"nkParseFunction">> => <<"
                    function() {
                        field = $$(this);
                        if (field && field.config && !field.config.readonly && this.config.suggest) {
                            suggest = $$(field.config.suggest);
                            if (suggest) {
                                suggest.show(this.getInputNode());
                            }
                        }
                    }
                ">>
            }
        }
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

body_form_row(#{type:=View, id:=Id, label:=Label, value:=Value, options:=Options}=Spec)
when View =:= suggest orelse View =:= multisuggest ->
    Base = get_base_form_row(Spec),
    Spec2 = Spec#{
        onItemClick => <<"
            function() {
                field = $$(this);
                if (field && field.config && !field.config.readonly && this.config.suggest) {
                    suggest = $$(field.config.suggest);
                    if (suggest) {
                        suggest.show(this.getInputNode());
                    }
                }
            }
        ">>
    },
    Base2 = nkadmin_util:add_listeners([onItemClick | get_form_listeners()], Spec2, Base),
    Base3 = case View of
        suggest ->
            Base2#{
                suggest => Options
            };
        multisuggest ->
            Base2#{
                suggest => #{
                    view => <<"multisuggest">>,
                    button => false,
                    data => Options
                }
            }
    end,
    Base3#{
        view => <<"text">>,
        %%id => Id,
        name => Id,
        label => Label,
        placeholder => placeholder(Label),
        labelWidth => 150,
        labelAlign => <<"left">>,
        value => Value
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
        css => <<"nk_read_only_field">>,
        value => Value,
        disabled => false,
        readonly => true,
        on => #{
            onBeforeRender => #{
                nkParseFunction => js_fun_date()
            }
        }
    };

body_form_row(#{type:=button, id:=Id, label:=Label, onClick:=OnClick}=Spec) ->
    Base = get_base_form_row(Spec),
    ButtonType = maps:get(button_type, Spec, <<"form">>),
    ButtonIcon = maps:get(button_icon, Spec, <<>>),
    Value = maps:get(value, Spec, false),
    Base2 = case ButtonIcon of
        <<>> ->
            Base;
        _ ->
            Base#{icon => ButtonIcon}
    end,
    Base2#{
        view => <<"button">>,
        %%id => Id,
        name => Id,
        type => ButtonType,
        label => Label,
        value => Value,
        click => #{
            <<"nkParseFunction">> => OnClick
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
            }).then(function(response) {
                console.log('Action ", Action/binary, " button clicked OK: ', response);
                if (response.data && response.data.elements) {
                    updateView(response.data.elements);
                }
            }).catch(function(response) {
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
                }).then(function(response) {
                    console.log('Action ", Action/binary, " button clicked OK: ', response);
                    if (response.data && response.data.elements) {
                        updateView(response.data.elements);
                    }
                }).catch(function(response) {
                    console.log('Action ", Action/binary, " error: ', response);
                    webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error});
                });
        }
    }
">>.


%% @doc
js_fun_icon_upload_form(FormId, Opts) ->
    Action = <<"save">>,
    Success = <<"function(e, xhr) {
        var form = $$(\"", FormId/binary, "\");
        if (form && form.validate()) {
            var values = form.getValues({disabled:true, hidden:true});
            var response = JSON.parse(xhr.responseText);
            values.icon_id = response.obj_id;
            ncClient.sendMessageAsync(
                \"objects/admin.session/element_action\",
                {
                    element_id: \"", FormId/binary, "\",
                    action: \"", Action/binary, "\",
                    value: values
                }).then(function(response) {
                    console.log('Action ", Action/binary, " button clicked OK: ', response);
                    if (response.data && response.data.elements) {
                        updateView(response.data.elements);
                    }
                }).catch(function(response) {
                    console.log('Action ", Action/binary, " error: ', response);
                    webix.message({ 'type': 'error', 'text': response.data.code + ' - ' + response.data.error});
                });
        }
    }">>,
    create_upload_onclick(FormId, Opts#{on_success => Success}).


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
    Base2 = case Disabled of
        true ->
            Base#{css => <<"nk_read_only_field">>};
        false ->
            Base
    end,
    Base2#{
        required => Required,
        hidden => HiddenValue,
        readonly => DisabledValue
        %disabled => DisabledValue
    }.


%% @private
get_form_listeners() ->
    [onChange].


%% @private
get_js_function(<<>>) ->
    <<"function() {}">>;

get_js_function(Function) ->
    Function.


%% @private
get_js_function(<<>>, Default) ->
    Default;

get_js_function(Function, _Default) ->
    Function.



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