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
        name => binary(),
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


-type opts() ::
    #{
        language => binary(),
        columns => [column()],
        on_click => [on_click()],
        body_id => binary(),
        domain_id => binary(),
        filters => [binary()]
    }.

-define(BIN(X), (to_bin(X))/binary).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec datatable(opts(), nkadmin_session_obj:session()) ->
    map().

datatable(Opts, Session) ->
    BodyId = maps:get(body_id, Opts, <<"body">>),
    #{
        view => <<"scrollview">>,
        id => <<?BIN(BodyId)>>,
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
                        toolbar(Opts, Session),
                        #{
                            rows => [
                                body_data(Opts, Session),
                                body_pager()
                            ]
                        }
                    ]
                }
            ]
        }
    }.


toolbar(Opts, Session) ->
    #{
        height => 40,
        cols => [
            toolbar_show_subdomains(Opts, Session),
            #{},
            toolbar_selected_elements(Opts, Session),
            #{},
            toolbar_real_time(Opts, Session),
            toolbar_refresh(Opts, Session),
            toolbar_new(Opts, Session),
            toolbar_delete(Opts, Session),
            toolbar_disable(Opts, Session)
        ]
    }.

toolbar_selected_elements(#{table_id:=TableId}, _Session) ->
    SelectedId = <<?BIN(TableId),"_selected">>,
    SelectedIconId = <<?BIN(SelectedId),"_icon">>,
    SelectedLabelId = <<?BIN(SelectedId),"_label">>,
    RealTimeButtonId = <<?BIN(TableId),"_real_time">>,
    RefreshButtonId = <<?BIN(TableId),"_refresh">>,
    NewButtonId = <<?BIN(TableId),"_new">>,
    DeleteIconId = <<?BIN(TableId),"_delete">>,
    DisableIconId = <<?BIN(TableId),"_disable">>,
    #{
        view => <<"layout">>,
        id => <<?BIN(SelectedId)>>,
        borderless => true,
        hidden => true,
        cols => [
            #{
                id => <<?BIN(SelectedIconId)>>,
                width => 30,
                css => <<"datatable_icon">>,
                template => <<"
                    <span style='cursor:pointer;' class='webix_icon fa-times'></span>
                ">>,
                onClick => #{
                    <<"fa-times">> => <<"
                        function() {
                            // Clear datatable selection
                            var masterCheckbox = $$('", ?BIN(TableId), "').getHeaderContent('checkbox');
                            if (masterCheckbox) {
                                console.log('masterCheckbox found!', masterCheckbox);
                                masterCheckbox.uncheck();
                            } else {
                                console.log('masterCheckbox not found');
                            }
                            // There aren't any object selected
                            // Hide possible actions
                            $$('",?BIN(SelectedId),"').hide();
                            $$('",?BIN(DeleteIconId),"').hide();
                            $$('",?BIN(DisableIconId),"').hide();
                            // Show normal buttons
                            $$('",?BIN(RealTimeButtonId),"').show();
                            $$('",?BIN(RefreshButtonId),"').show();
                            $$('",?BIN(NewButtonId),"').show();
                        }
                    ">>
                }
            },
            #{
                view => <<"label">>,
                id => <<?BIN(SelectedLabelId)>>,
                autowidth => true,
                data => #{text => <<" items selected">>},
                % This label is defined separately to be able to set its width to 'autowidth'
                %label => nkadmin_util:i18n(domain_show_subdomains, Opts)
                label => <<"
                    0 items selected
                ">>
            }
        ]
    }.

toolbar_refresh(#{table_id:=TableId}=Opts, Session) ->
    #{
        view => <<"button">>,
        id => <<?BIN(TableId),"_refresh">>,
        type => <<"iconButton">>,
        icon => <<"refresh">>,
        autowidth => true,
        label => nkadmin_util:i18n(domain_refresh, Session),
        click => fake_delay(Opts)
    }.


toolbar_real_time(#{table_id:=TableId}, Session) ->
    #{
        view => <<"layout">>,
        id => <<?BIN(TableId),"_real_time">>,
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
                            grid.clearAll();
                            // Subscribe/Unsubscribe to real time updates
                        }
                    ">>
                }
            },
            #{
                view => <<"label">>,
                autowidth => true,
                % This label is defined separately to be able to set its width to 'autowidth'
                label => nkadmin_util:i18n(real_time, Session)
                %align => <<"right">>
            }
        ]
    }.

toolbar_new(#{table_id:=TableId}=Opts, _Session) ->
    #{
        view => <<"button">>,
        id => <<?BIN(TableId),"_new">>,
        type => <<"iconButton">>,
        icon => <<"plus">>,
        autowidth => true,
        label => <<"New">>,
        click => fake_delay(Opts)
    }.


toolbar_show_subdomains(#{table_id:=TableId, subdomains_id:=SubdomainsId}, Session) ->
    #{
        view => <<"layout">>,
        cols => [
            #{
                id => <<SubdomainsId/binary>>,
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
                label => nkadmin_util:i18n(domain_show_subdomains, Session)
                %align => <<"right">>
            }
        ]
    }.

toolbar_delete(#{table_id:=TableId}, _Session) ->
    #{
        id => <<?BIN(TableId),"_delete">>,
        width => 30,
        hidden => true,
        css => <<"datatable_icon">>,
        template => <<"
           <span style='cursor:pointer;' class='webix_icon fa-trash'></span>
        ">>
    }.

toolbar_disable(#{table_id:=TableId}, _Session) ->
    #{
        id => <<?BIN(TableId),"_disable">>,
        width => 30,
        hidden => true,
        css => <<"datatable_icon">>,
        template => <<"
           <span style='cursor:pointer;' class='webix_icon fa-ban'></span>
        ">>
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


body_data(#{table_id:=TableId}=Opts, #admin_session{domain_id=DomainId}=Session) ->
    OnMasterCheckboxClick = on_master_checkbox_click(Opts),
    #{
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
        leftSplit => maps:get(left_split, Opts, 0),
        rightSplit => maps:get(right_split, Opts, 0),
        navigation => true,
        nkFilters => maps:get(filters, Opts, []),
        nkDomain => DomainId,
        columns => make_columns(Opts, Session),
        pager => <<"pagerA">>,
        export => true,
        url => <<"wsProxy->">>,
        save => <<"wsProxy->">>,
        onClick => make_on_click(Opts),
        ready => <<"
            function() {
                webix.extend(this, webix.ProgressBar);
                var grid = $$('", ?BIN(TableId), "');
                var masterCheckbox = grid.getHeaderContent('checkbox');
                if (masterCheckbox) {
                    // attach an event to customMasterCheckbox;
                    masterCheckbox.setOnClickListener(", ?BIN(OnMasterCheckboxClick), ");
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
        onClick => #{
            webix_silent_checkbox => <<"
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
        },
        on => #{
            <<"onBeforeLoad">> => on_before_load(Opts),
            <<"onCheck">> => on_check(Opts),
            <<"data->onStoreUpdated">> => on_store_updated()
        }
    }.



%% @private
make_columns(#{columns:=Columns}=Opts, Session) ->
    make_columns(Columns, Opts, [], Session).


%% @private
make_columns([], _Opts, Acc, _Session) ->
    lists:reverse(Acc);

make_columns([#{id:=Id, type:=Type}=Column|Rest], Opts, Acc, Session) ->
    Name = maps:get(name, Column, <<"&nbsp;">>),
    Data1 = column(Id, Type, Name, Column, Session),
    Data2 = column_opts(Data1, Column),
    make_columns(Rest, Opts, [Data2|Acc], Session).



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
    case FilterOptions of
        [] -> Filter = #{ content => <<"serverFilter">>, colspan => FilterColspan };
        _ -> Filter = #{ content => <<"serverSelectFilter">>, colspan => FilterColspan, options => FilterOptions }
    end,
    IsHtml = maps:get(is_html, Column, false),
    case IsHtml of
        false ->
            % #!column_id# -> the "!" enforces data escaping
            Template = <<"<span class=\"", ?BIN(Id), "\">#!", ?BIN(Id) ,"#</span>">>;
        true ->
            Template = <<"<span class=\"", ?BIN(Id), "\">#", ?BIN(Id) ,"#</span>">>
    end,
    #{
        id => Id,
        header => [
            #{ text => nkadmin_util:i18n(Name, Session), colspan => HeaderColspan },
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
            #{ text => nkadmin_util:i18n(Name, Session), colspan => HeaderColspan },
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
           <span style='cursor:pointer;' class='webix_icon #", ?BIN(Icon), "#'></span>
        ">>
    };

column(Id, {fixed_icon, Icon}, _Name, _Column, _Session) ->
    #{
        id => Id,
        header => <<"&nbsp;">>,
        width => 30,
        template => <<"
           <span style='cursor:pointer;' class='webix_icon ", ?BIN(Icon), "'></span>
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
make_on_click(#{on_click:=OnClick}=Opts) ->
    make_on_click(OnClick, Opts, #{}).


%% @private
make_on_click([], _Opts, Acc) ->
    Acc;

make_on_click([#{id:=Id, type:=Type}=OnClick|Rest], Opts, Acc) ->
    Acc2 = on_click(Id, Type, OnClick, Opts, Acc),
    make_on_click(Rest, Opts, Acc2).


%% @private
on_click(Id, delete, _OnClick, #{table_id:=TableId}=_Opts, Acc) ->
    Acc#{
        Id => <<"
            function(e, id, node) {
                webix.confirm({
                    \"text\": \"This object will be deleted. <br/> Are you sure?\",
                    \"ok\": \"Yes\",
                    \"cancel\": \"Cancel\",
                    \"callback\": function(res) {
                        if(res) {
                            webix.$$(\"", TableId/binary, "\").remove(id);
                        }
                    }
                });
            }
        ">>
    };

on_click(Id, disable, _OnClick, #{table_id:=TableId}=_Opts, Acc) ->
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
                            var grid = webix.$$(\"", TableId/binary, "\");
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

on_click(Id, enable, _OnClick, #{table_id:=TableId}=_Opts, Acc) ->
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
                            var grid = webix.$$(\"", TableId/binary, "\");
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
    }.


%% @private
on_before_load(#{table_id:=_TableId}=_Opts) ->
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
on_check(_Opts) ->
    <<"
        function(row, col, val, ignore){
            if (ignore !== undefined || ignore) {
                //console.log('Checkbox ignored');
            } else {
                console.log('Checkbox: ', row, col, val, ignore);
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

%% @private
on_master_checkbox_click(_Opts) ->
    <<"
        function(value) {
            console.log('Master checkbox: ', value);
        }
    ">>.

%% @private
fake_delay(#{table_id:=TableId}=_Opts) ->
    SelectedId = <<?BIN(TableId),"_selected">>,
    RealTimeButtonId = <<?BIN(TableId),"_real_time">>,
    RefreshButtonId = <<?BIN(TableId),"_refresh">>,
    NewButtonId = <<?BIN(TableId),"_new">>,
    DeleteIconId = <<?BIN(TableId),"_delete">>,
    DisableIconId = <<?BIN(TableId),"_disable">>,
    <<"
        function() {
            var grid = $$(\"", TableId/binary, "\");
            grid.showProgress();
            webix.delay(function() {
                grid.hideProgress();
                // There are items selected
                // Show possible actions
                $$('", ?BIN(SelectedId), "').show();
                $$('", ?BIN(DeleteIconId), "').show();
                $$('", ?BIN(DisableIconId), "').show();
                // Hide normal buttons
                $$('", ?BIN(RealTimeButtonId), "').hide();
                $$('", ?BIN(RefreshButtonId), "').hide();
                $$('", ?BIN(NewButtonId), "').hide();
            }, null, null, 300);
        }
    ">>.



%% @private
to_bin(Bin) when is_binary(Bin) -> Bin;
to_bin(Term) -> nklib_util:to_binary(Term).