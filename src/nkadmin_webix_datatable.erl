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

-module(nkadmin_webix_datatable).
-export([datatable/2]).

-include("nkadmin.hrl").

%% ===================================================================
%% Types
%% ===================================================================

-type column() ::
    #{
        id => binary(),
        type => pos | text | date | {icon, binary()} | {fixed_icon, binary()},
        name => binary(),                   % For i18n
        options => filter_options(),
        header_colspan => integer(),
        filter_colspan => integer(),
        fillspace => integer(),
        editor => text,
        is_html => boolean(),
        sort => boolean()
    }.

-type filter_options() ::
    #{
        id => binary(),
        value => binary()
    }.

-type on_click() ::
    #{
        id => binary(),
        type => enable | disable | delete
    }.


-type spec() ::
    #{
        table_id => binary(),           %% Mandatory
        filters => [binary()],
        left_split => integer(),
        right_split => integer(),
        on_click => [on_click()],
        columns => [column()]
    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec datatable(spec(), nkadmin_session_obj:session()) ->
    map().

datatable(#{table_id:=TableId}=Spec, Session) ->
    BodyId = case Spec of
        #{is_subtable:=true} ->
            <<TableId/binary, "_body">>;
        _ ->
            <<"body">>
    end,
    Header = maps:get(header, Spec, <<>>),
    #{
        view => <<"scrollview">>,
        id => BodyId,
        header => Header,
        borderless => true,
        type => <<"space">>,
        css => <<"flex-tmp">>,
        scroll => <<"xy">>,
        body => #{
            rows => [
                #{
                    type => <<"space">>,
                    minHeight => 300,
                    minWidth => 400,
                    rows => [
                        toolbar(TableId, Session),
                        #{
                            rows => [
                                body_data(TableId, Spec, Session),
                                body_pager()
                            ]
                        }
                    ]
                }
            ]
        }
    }.


toolbar(TableId, Session) ->
    #{
        height => 40,
        cols => [
            toolbar_show_subdomains(TableId, Session),
            #{},
            toolbar_selected_elements(TableId, Session),
            #{},
            toolbar_real_time(TableId, Session),
            toolbar_refresh(TableId, Session),
            toolbar_new(TableId),
            toolbar_delete(TableId, Session),
            toolbar_disable(TableId, Session)
        ]
    }.


toolbar_show_subdomains(TableId, Session) ->
    SubdomainsId = append_id(TableId, <<"_subdomains">>),
    #{
        view => <<"layout">>,
        cols => [
            #{
                id => SubdomainsId,
                view => <<"checkbox">>,
                name => <<"show_subdomains_checkbox">>,
                width => 20,
                value => 1,
                on => #{
                    onChange => <<"
                        function() {
                            var grid = $$(\"", TableId/binary, "\");
                            var pager = grid.getPager();
                            var page = pager.config.page;
                            var start = page * pager.config.size;
                            console.log('grid', grid);
                            //grid.loadNext(number count,number start,function callback,string url,boolean now);
                            grid.clearAll();
                            grid.loadNext(grid.config.datafetch, 0, null, grid.config.url, true);
                        }
                    ">>
                }
            },
            #{
                view => <<"label">>,
                autowidth => true,
                % This label is defined separately to be able to set its width to 'autowidth'
                label => i18n(domain_show_subdomains, Session)
                %align => <<"right">>
            }
        ]
    }.


toolbar_selected_elements(TableId, Session) ->
    SelectedId = append_id(TableId, <<"_selected">>),
    SelectedIconId = append_id(SelectedId, <<"_icon">>),
    SelectedLabelId = append_id(SelectedId, <<"_label">>),
    #{
        view => <<"layout">>,
        id => SelectedId,
        borderless => true,
        hidden => true,
        cols => [
            #{
                id => SelectedIconId,
                width => 30,
                css => <<"datatable_icon">>,
                template => <<"
                    <span style='cursor:pointer;' class='webix_icon fa-times'></span>
                ">>,
                onClick => #{
                    <<"fa-times">> => <<"
                        function() {
                            // Clear datatable selection
                            var masterCheckbox = $$('", TableId/binary, "').getHeaderContent('checkbox');
                            if (masterCheckbox) {
                                console.log('masterCheckbox found!', masterCheckbox);
                                masterCheckbox.uncheck();
                            } else {
                                console.log('masterCheckbox not found');
                            }
                        }
                    ">>
                }
            },
            #{
                view => <<"label">>,
                id => SelectedLabelId,
                autowidth => true,
                data => #{text => i18n(domain_items_selected, Session)},
                % This label is defined separately to be able to set its width to 'autowidth'
                label => i18n(domain_items_selected, Session)
            }
        ]
    }.

toolbar_refresh(TableId, Session) ->
    #{
        view => <<"button">>,
        id => append_id(TableId, <<"_refresh">>),
        type => <<"iconButton">>,
        icon => <<"refresh">>,
        autowidth => true,
        label => i18n(domain_refresh, Session)
%        click => fake_delay(TableId)
    }.


toolbar_real_time(TableId, Session) ->
    #{
        view => <<"layout">>,
        id => append_id(TableId, <<"_real_time">>),
        cols => [
            #{
                view => <<"checkbox">>,
                name => <<"real_time_checkbox">>,
                width => 20,
                value => 1,
                on => #{
                    onChange => <<"
                        function() {
                            var grid = $$(\"", TableId/binary, "\");
                            // Subscribe/Unsubscribe to real time updates
                        }
                    ">>
                }
            },
            #{
                view => <<"label">>,
                autowidth => true,
                % This label is defined separately to be able to set its width to 'autowidth'
                label => i18n(real_time, Session)
                %align => <<"right">>
            }
        ]
    }.

toolbar_new(TableId) ->
    #{
        view => <<"button">>,
        id => append_id(TableId, <<"_new">>),
        type => <<"iconButton">>,
        icon => <<"plus">>,
        autowidth => true,
        label => <<"New">>
%        click => fake_delay(TableId)
    }.



toolbar_delete(TableId, Session) ->
    Tooltip = i18n(domain_delete_button_tooltip, Session),
    #{
        id => append_id(TableId, <<"_delete">>),
        width => 30,
        hidden => true,
        css => <<"datatable_icon">>,
        template => <<"
           <span title='", Tooltip/binary, "' style='cursor:pointer;' class='webix_icon fa-trash'></span>
        ">>,
        onClick => #{
            <<"fa-trash">> => <<"
                function() {
                    // Delete selection
                    var grid = $$('", TableId/binary, "');
                    if (grid) {
                        console.log('delete button clicked!', grid.selectionCounter, grid.selectedItems);
                    }
                }
            ">>
        }
    }.

toolbar_disable(TableId, Session) ->
    Tooltip = i18n(domain_disable_button_tooltip, Session),
    #{
        id => append_id(TableId, <<"_disable">>),
        width => 30,
        hidden => true,
        css => <<"datatable_icon">>,
        template => <<"
           <span title='", Tooltip/binary, "' style='cursor:pointer;' class='webix_icon fa-ban'></span>
        ">>,
        onClick => #{
            <<"fa-ban">> => <<"
                function() {
                    // Disable selection
                    var grid = $$('", TableId/binary, "');
                    if (grid) {
                        console.log('disable button clicked!', grid.selectionCounter, grid.selectedItems);
                    }
                }
            ">>
        }
    }.

body_pager() ->
    #{
        view => <<"pager">>,
        id => <<"pagerA">>,
        template => <<"
            {common.first()}{common.prev()}&nbsp; {common.pages()}&nbsp; {common.next()}{common.last()}&nbsp;Total:&nbsp;#count#
        ">>,
        align => <<"center">>,
        autosize => true,
        height => 35,
        group => 5
    }.


body_data(TableId, Spec, #admin_session{domain_id=DomainId}=Session) ->
    OnMasterCheckboxClick = on_master_checkbox_click(TableId),
    Data = #{
        id => TableId,
        view => <<"datatable">>,
        css => <<"nk_datatable">>,
        select => <<"row">>,
        %        multiselect => true,
        dragColumn => true,
        resizeColumn => true,
        editable => true,
        editaction => <<"dblclick">>,
        scrollX => true,
        leftSplit => maps:get(left_split, Spec, 0),
        rightSplit => maps:get(right_split, Spec, 0),
        navigation => true,
        nkFilters => maps:get(filters, Spec, []),
        nkDomain => to_bin(DomainId),
        columns => make_columns(Spec, Session),
        pager => <<"pagerA">>,
        export => true,
        url => <<"wsProxy->">>,
        save => <<"wsProxy->">>,
        % onClick => make_on_click(TableId, Spec),
        ready => <<"
            function() {
                webix.extend(this, webix.ProgressBar);
                var grid = $$('", TableId/binary, "');
                var masterCheckbox = grid.getHeaderContent('checkbox');
                grid.selectedItems = {};
                grid.selectionCounter = 0;
                if (masterCheckbox) {
                    // attach an event to customMasterCheckbox;
                    masterCheckbox.setOnClickListener(", OnMasterCheckboxClick/binary, ");
                }
            }
        ">>,
        type => #{
            silentCheckbox => <<"
                function(obj, common, value, config) {
                    var checked = (value == config.checkValue) ? 'checked=\"true\"' : '';
                    return \"<input class='webix_silent_checkbox' type='checkbox' \"+checked+\">\";
                }
        ">>
        },
        on => #{
            <<"onBeforeLoad">> => on_before_load(),
            <<"onCheck">> => on_check(TableId),
            <<"data->onStoreUpdated">> => on_store_updated()
        }
    },
    #{on_click:=OnClick} = Spec,
    Spec2 = Spec#{on_click := [#{id => webix_silent_checkbox, type => silent_checkbox } | OnClick]},
    add_on_click(TableId, Spec2, Data).



%% @private
make_columns(#{columns:=Columns}=Spec, Session) ->
    make_columns(Columns, Spec, [], Session).


%% @private
make_columns([], _Spec, Acc, _Session) ->
    lists:reverse(Acc);

make_columns([#{id:=Id, type:=Type}=Column|Rest], Spec, Acc, Session) ->
    Name = maps:get(name, Column, <<"&nbsp;">>),
    Data1 = column(to_bin(Id), Type, Name, Column, Session),
    Data2 = column_opts(Data1, Column),
    make_columns(Rest, Spec, [Data2|Acc], Session).



%% @private
column(Id, pos, _Name, Column, _Session) ->
    HeaderColspan = maps:get(header_colspan, Column, <<"1">>),
    #{
        id => Id,
        header => #{ text => <<"#">>, colspan => HeaderColspan },
        width => 50
    };

column(Id, checkbox, _Name, _Column, _Session) ->
    #{
        id => Id,
        header => #{
            content => <<"customMasterCheckbox">>,
            css => <<"master_checkbox">>,
            contentId => <<"checkbox">>
        },
        css => <<"center checkbox">>,
        template => <<"{common.silentCheckbox()}">>,
        width => 40
    };

column(Id, text, Name, Column, Session) ->
    HeaderColspan = maps:get(header_colspan, Column, <<"1">>),
    FilterColspan = maps:get(filter_colspan, Column, <<"1">>),
    FilterOptions = maps:get(options, Column, []),
    Fillspace = maps:get(fillspace, Column, <<"1">>),
    Filter = case FilterOptions of
        [] ->
            #{
                content => <<"serverFilter">>,
                colspan => FilterColspan
            };
        _ ->
            #{
                content => <<"serverSelectFilter">>,
                colspan => FilterColspan,
                options => FilterOptions
            }
    end,
    IsHtml = maps:get(is_html, Column, false),
    case IsHtml of
        false ->
            % #!column_id# -> the "!" enforces data escaping
            Template = <<"<span class=\"", Id/binary, "\">#!", Id/binary ,"#</span>">>;
        true ->
            Template = <<"<span class=\"", Id/binary, "\">#", Id/binary ,"#</span>">>
    end,
    #{
        id => Id,
        header => [
            #{ text => i18n(Name, Session), colspan => HeaderColspan },
            Filter
        ],
        template => Template,
        fillspace => Fillspace,
        minWidth => <<"100">>
    };

column(Id, date, Name, Column, Session) ->
    HeaderColspan = maps:get(header_colspan, Column, <<"1">>),
    FilterColspan = maps:get(filter_colspan, Column, <<"1">>),
    Fillspace = maps:get(fillspace, Column, <<"1">>),
    CreateOptions = [
        #{ id => <<"">>, value => <<"">> },
        #{ id => <<"today">>, value => <<"Today">> },
        #{ id => <<"yesterday">>, value => <<"Yesterday">> },
        #{ id => <<"last_7">>, value => <<"Last 7 days">> },
        #{ id => <<"last_30">>, value => <<"Last 30 days">> },
        #{ id => <<"custom">>, value => <<"Custom">> }
    ],
    #{
        id => Id,
        header => [
            #{ text => i18n(Name, Session), colspan => HeaderColspan },
            #{ content => <<"serverSelectFilter">>, colspan => FilterColspan, options => CreateOptions }
        ],
        fillspace => Fillspace,
        minWidth => <<"100">>,
        format => <<"
            function(value) {   // 'en-US', 'es-ES', etc.
                    return (new Date(value)).toLocaleString();
            }
        ">>
    };

column(Id, {icon, Icon}, _Name, _Column, _Session) ->
    #{
        id => Id,
        header => <<"&nbsp;">>,
        width => 30,
        template => <<"
           <span style='cursor:pointer;' class='webix_icon #", Icon/binary, "#'></span>
        ">>
    };

column(Id, {fixed_icon, Icon}, _Name, _Column, _Session) ->
    #{
        id => Id,
        header => <<"&nbsp;">>,
        width => 30,
        template => <<"
           <span style='cursor:pointer;' class='webix_icon ", Icon/binary, "'></span>
        ">>
    }.


%% @private
column_opts(Data, Column) ->
    Data2 = case Column of
        #{sort := true} ->
            Data#{sort => <<"server">>};
        _ ->
            Data
    end,
    case Column of
        #{editor := text} ->
            Data2#{editor => <<"text">>};
        _ ->
            Data2
    end.


%% @private
add_on_click(TableId, #{on_click:=OnClick}, Data) ->
    Data#{onClick => make_on_click(TableId, OnClick, #{})};

add_on_click(_TableId, _Spec, Data) ->
    Data.


%% @private
make_on_click(_TableId, [], Acc) ->
    Acc;

make_on_click(TableId, [#{id:=Id, type:=Type}=OnClick|Rest], Acc) ->
    Acc2 = on_click(TableId, to_bin(Id), Type, OnClick, Acc),
    make_on_click(TableId, Rest, Acc2).


%% @private
on_click(TableId, Id, delete, _OnClick, Acc) ->
    Acc#{
        Id => <<"
            function(e, id, node) {
                webix.confirm({
                    \"text\": \"This object will be deleted. <br/> Are you sure?\",
                    \"ok\": \"Yes\",
                    \"cancel\": \"Cancel\",
                    \"callback\": function(res) {
                        if(res) {
                            $$(\"", TableId/binary, "\").remove(id);
                        }
                    }
                });
            }
        ">>
    };

on_click(TableId, Id, disable, _OnClick, Acc) ->
    Acc#{
        Id => <<"
            function(e, id, node) {
                // id === { row: obj_id, column: column_id };
                webix.confirm({
                    \"text\": \"This object will be disabled. <br/> Are you sure?\",
                    \"ok\": \"Yes\",
                    \"cancel\": \"Cancel\",
                    \"callback\": function(res) {
                        if (res) {
                            var grid = $$(\"", TableId/binary, "\");
                            ncClient.sendMessageAsync(\"objects/admin.session/element_action\", {
                                element_id: \"", TableId/binary, "\",
                                action: \"disabled\",
                                value: {
                                    obj_id: id.row
                                }
                            });
                            // Remove this lines (the state of the row will be changed by an event)
                            if(grid) {
                                grid.addRowCss(id, \"webix_cell_disabled\");
                                grid.getItem(id).enabled_icon = \"fa-check\";
                                grid.refresh(id);
                            }
                        }
                    }
                });
            }
        ">>
    };

on_click(TableId, Id, enable, _OnClick, Acc) ->
    Acc#{
        Id => <<"
            function(e, id, node) {
                // id === { row: obj_id, column: column_id };
                webix.confirm({
                    text: \"This object will be enabled. <br/> Are you sure?\",
                    ok: \"Yes\",
                    cancel: \"Cancel\",
                    callback: function(res) {
                        if (res) {
                            var grid = $$(\"", TableId/binary, "\");
                            ncClient.sendMessageAsync(\"objects/admin.session/element_action\", {
                                element_id: \"", TableId/binary, "\",
                                action: \"enabled\",
                                value: {
                                    obj_id: id.row
                                }
                            });
                            // Remove this lines (the state of the row will be changed by an event)
                            if(grid) {
                                grid.removeRowCss(id, \"webix_cell_disabled\");
                                grid.getItem(id).enabled_icon = \"fa-times\";
                                grid.refresh(id);
                            }
                        }
                    }
                });
            }
        ">>
    };

on_click(_TableId, Id, silent_checkbox, _OnClick, Acc) ->
    Acc#{
        Id => <<"
            function(e, id) {
                // same code uses the default checkbox, but the update part is removed
                id = this.locate(e);
                
                var item = this.getItem(id.row);
                var col = this.getColumnConfig(id.column);
                var trg = e.target|| e.srcElement;
                var check = (trg.type == 'checkbox')? trg.checked : (item[id.column] != col.checkValue);
                var value = check ? col.checkValue : col.uncheckValue;
                // Save this change silently into the cached data (without sending this change to the server)
                item[id.column] = value;
                this.refresh(id.row);
                // Call the onCheck listener
                this.callEvent('onCheck', [id.row, id.column, value]);
                return false;
            }
        ">>
    }.


%% @private
on_before_load() ->
    <<"
        function() {
            webix.ui.datafilter.customFilter2 = {
                refresh: function(master, node, column) {
                    node.onchange = function() {};
                    node.onclick = function(e) {
                        // Prevent the column from changing the order when clicking the filter
                        e.stopPropagation();
                    };
                },
                render: function(a, b) {
                    return  \"<select style='width:100%; height:25px; font-family:Verdana'; id=\"+b.columnId+\">\" +
                            \"<option>Old</option>\" +
                            \"<option>New</option>\" +
                            \"</select>\";
                }
            };
            webix.ui.datafilter.extendedFilter2 = webix.extend({
                refresh:function(master, node, column){
                    //event handlers
                    node.onclick = function(e) {
                        // Prevent the column from changing the order when clicking the filter
                        e.stopPropagation();
                    };
                    node.onkeyup = function(){
                        let input = this.children[0].children[0];
                        if (input.prevValue !== input.value) {
                            console.log('Filter ' + column.columnId + ' changed: ' + input.value);
                            master.clearAll();
                            let newObj =
                            {
                                id: 1,
                                uuid: 123456789,
                                parentUuid: 987654321,
                                type: 0,
                                typeName: \"User\",
                                shortName: \"user\",
                                enabled: true,
                                enabledIcon: \"fa-check\"
                            };
                            if (column.columnId === 'id') {
                                newObj.id = input.value;
                            } else if (column.columnId === 'uuid') {
                                newObj.uuid = input.value;
                            } else if (column.columnId === 'parentUuid') {
                                newObj.parentUuid = input.value;
                            } else if (column.columnId === 'typeName') {
                                newObj.typeName = input.value;
                            } else if (column.columnId === 'shortName') {
                                newObj.shortName = input.value;
                            }
                            master.add(newObj, 0);
                        };
                        input.prevValue = input.value;
                    }
                }
            }, webix.ui.datafilter.textFilter);
        }
    ">>.

%% @private
on_check(TableId) ->
    SelectedId = append_id(TableId, <<"_selected">>),
    SelectedLabelId = append_id(SelectedId, <<"_label">>),
    RealTimeButtonId = append_id(TableId, <<"_real_time">>),
    RefreshButtonId = append_id(TableId, <<"_refresh">>),
    NewButtonId = append_id(TableId, <<"_new">>),
    DeleteIconId = append_id(TableId, <<"_delete">>),
    DisableIconId = append_id(TableId, <<"_disable">>),
    <<"
        function(row, col, val, fromMaster){
            console.log('Checkbox: ', row, col, val, fromMaster);
            var grid = $$('", TableId/binary, "');
            if (grid) {
                if (val) {
                    grid.selectionCounter += 1;
                    grid.selectedItems[row] = row;
                } else {
                    grid.selectionCounter = grid.selectionCounter > 0? grid.selectionCounter - 1 : 0;
                    delete grid.selectedItems[row];
                }
                console.log('Grid selection counter: ', grid.selectionCounter);
                if (fromMaster === undefined || !fromMaster) {
                    // Update the view only if this event wasn't started by a master checkbox click
                    if (grid.selectionCounter > 0) {
                        // There are items selected
                        // Modify the counter
                        var label = $$('", SelectedLabelId/binary, "');
                        // TODO: Replace some special character with the counter instead of concatenating it with our string
                        label.setValue(grid.selectionCounter + label.data.data.text);
                        // Show possible actions
                        $$('", SelectedId/binary, "').show();
                        $$('", DeleteIconId/binary, "').show();
                        $$('", DisableIconId/binary, "').show();
                        // Hide normal buttons
                        $$('", RealTimeButtonId/binary, "').hide();
                        $$('", RefreshButtonId/binary, "').hide();
                        $$('", NewButtonId/binary, "').hide();
                    } else {
                        // There aren't any items selected
                        // Hide possible actions
                        $$('", SelectedId/binary, "').hide();
                        $$('", DeleteIconId/binary, "').hide();
                        $$('", DisableIconId/binary, "').hide();
                        // Show normal buttons
                        $$('", RealTimeButtonId/binary, "').show();
                        $$('", RefreshButtonId/binary, "').show();
                        $$('", NewButtonId/binary, "').show();
                    }
                }
            }
        }
    ">>.

%% @private
on_master_checkbox_click(TableId) ->
    SelectedId = append_id(TableId, <<"_selected">>),
    SelectedLabelId = append_id(SelectedId, <<"_label">>),
    RealTimeButtonId = append_id(TableId, <<"_real_time">>),
    RefreshButtonId = append_id(TableId, <<"_refresh">>),
    NewButtonId = append_id(TableId, <<"_new">>),
    DeleteIconId = append_id(TableId, <<"_delete">>),
    DisableIconId = append_id(TableId, <<"_disable">>),
    <<"
        function(value, counter) {
            console.log('Master checkbox: ', value, counter);
            // This event will update the view once
            if (value && counter > 0) {
                // Modify the counter
                var label = $$('", SelectedLabelId/binary, "');
                // TODO: Replace some special character with the counter instead of concatenating it with our string
                label.setValue(counter + label.data.data.text);
                // Show possible actions
                $$('", SelectedId/binary, "').show();
                $$('", DeleteIconId/binary, "').show();
                $$('", DisableIconId/binary, "').show();
                // Hide normal buttons
                $$('", RealTimeButtonId/binary, "').hide();
                $$('", RefreshButtonId/binary, "').hide();
                $$('", NewButtonId/binary, "').hide();
            } else {
                // There aren't any items selected
                // Hide possible actions
                $$('", SelectedId/binary, "').hide();
                $$('", DeleteIconId/binary, "').hide();
                $$('", DisableIconId/binary, "').hide();
                // Show normal buttons
                $$('", RealTimeButtonId/binary, "').show();
                $$('", RefreshButtonId/binary, "').show();
                $$('", NewButtonId/binary, "').show();                
            }
        }
    ">>.

%% @private
on_store_updated() ->
    <<"
        function() {
            this.data.each(function(obj, i) {
                if (obj !== undefined) {
                    obj.index = i+1;
                }
            })
        }
    ">>.

%%%% @private
%%fake_delay(TableId) ->
%%    SelectedId = append_id(TableId, <<"_selected">>),
%%    RealTimeButtonId = append_id(TableId, <<"_real_time">>),
%%    RefreshButtonId = append_id(TableId, <<"_refresh">>),
%%    NewButtonId = append_id(TableId, <<"_new">>),
%%    DeleteIconId = append_id(TableId, <<"_delete">>),
%%    DisableIconId = append_id(TableId, <<"_disable">>),
%%    <<"
%%        function() {
%%            var grid = $$(\"", TableId/binary, "\");
%%            grid.showProgress();
%%            webix.delay(function() {
%%                grid.hideProgress();
%%                // There are items selected
%%                // Show possible actions
%%                $$('", SelectedId/binary,  "').show();
%%                $$('", DeleteIconId/binary,  "').show();
%%                $$('", DisableIconId/binary,  "').show();
%%                // Hide normal buttons
%%                $$('", RealTimeButtonId/binary,  "').hide();
%%                $$('", RefreshButtonId/binary,  "').hide();
%%                $$('", NewButtonId/binary,  "').hide();
%%            }, null, null, 300);
%%        }
%%    ">>.



%% @private
append_id(TableId, Id) ->
    <<TableId/binary, "__", Id/binary>>.


i18n(<<"&nbsp;">>, _Session) -> <<>>;
i18n(Key, Session) -> nkadmin_util:i18n(Key, Session).


%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).
