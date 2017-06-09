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
-export([datatable/1]).

%% ===================================================================
%% Types
%% ===================================================================

-type column() ::
    #{
        id => binary(),
        type => text | date | {icon, binary()},
        name => binary()
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
        domain_id => binary(),
        filters => [binary()]
    }.

-define(BIN(X), (nklib_util:to_binary(X))/binary).


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec datatable(opts()) ->
    map().

datatable(Opts) ->
    #{
        view => <<"scrollview">>,
        id => <<"body">>,
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
                        toolbar(Opts),
                        #{
                            rows => [
                                body_data(Opts),
                                body_pager()
                            ]
                        }
                    ]
                }
            ]
        }
    }.


toolbar(Opts) ->
    #{
        height => 40,
        cols => [
            toolbar_refresh(Opts),
            toolbar_pause(Opts),
            toolbar_new(Opts),
            #{},
            toolbar_show_subdomains(Opts)
        ]
    }.


toolbar_refresh(Opts) ->
    #{
        view => <<"button">>,
        type => <<"iconButton">>,
        icon => <<"refresh">>,
        autowidth => true,
        label => nkadmin_util:i18n(domain_refresh, Opts),
        click => fake_delay()
    }.


toolbar_pause(_Opts) ->
    #{
        view => <<"button">>,
        type => <<"iconButton">>,
        icon => <<"pause">>,
        autowidth => true,
        label => <<"Pause">>,
        click => fake_delay()
    }.

toolbar_new(_Opts) ->
    #{
        view => <<"button">>,
        type => <<"iconButton">>,
        icon => <<"plus">>,
        autowidth => true,
        label => <<"New">>,
        click => fake_delay()
    }.


toolbar_show_subdomains(Opts) ->
    #{
        view => <<"layout">>,
        cols => [
            #{
                view => <<"label">>,
                autowidth => true,
                % This label is defined separately to be able to set its width to 'autowidth'
                label => nkadmin_util:i18n(domain_show_subdomains, Opts),
                align => <<"right">>
            },
            #{
                id => <<"objectsDataShowSubdomains">>,
                view => <<"checkbox">>,
                name => <<"show_subdomains_checkbox">>,
                width => 20,
                value => 1,
                on => #{
                    onChange => <<"
                        function() {
                            var grid = $$(\"domain_detail_user_table\");
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
            }
        ]
    }.


body_pager() ->
    #{
        view => <<"toolbar">>,
        css => <<"highlighted_header header6">>,
        paddingX => 5,
        paddingY => 5,
        height => 40,
        cols => [
            #{
                view => <<"pager">>,
                id => <<"pagerA">>,
                template => <<"
                    {common.first()}{common.prev()}&nbsp; {common.pages()}&nbsp; {common.next()}{common.last()}&nbsp;Total:&nbsp;#count#
                ">>,
                autosize => true,
                height => 35,
                group => 5
            }
        ]
    }.


body_data(Opts) ->
    #{
        id => <<"domain_detail_user_table">>,
        view => <<"datatable">>,
        select => true,
        dragColumn => true,
        editable => true,
        nkFilters => maps:get(filters, Opts, []),
        nkDomain => maps:get(domain_id, Opts),
        columns => make_columns(Opts),
        pager => <<"pagerA">>,
        export => true,
        url => <<"wsProxy->">>,
        save => <<"wsProxy->">>,
        onClick => make_on_click(Opts),
        ready => <<"
            function() {
                webix.extend(this, webix.ProgressBar);
            }
        ">>,
        on => #{
            <<"onBeforeLoad">> => on_before_load(Opts)
        }
    }.



%% @private
make_columns(#{columns:=Columns}=Opts) ->
    make_columns(Columns, Opts, []).


%% @private
make_columns([], _Opts, Acc) ->
    lists:reverse(Acc);

make_columns([#{id:=Id, type:=Type}=Column|Rest], Opts, Acc) ->
    Name = maps:get(name, Column, <<"&nbsp;">>),
    Data = column(Id, Type, Name, Column, Opts),
    make_columns(Rest, Opts, [Data|Acc]).



%% @private
column(Id, text, Name, _Column, Opts) ->
    #{
        id => Id,
        header => [nkadmin_util:i18n(Name, Opts), #{ content => <<"serverFilter">> }],
        fillspace => <<"1">>,
        sort => <<"server">>
    };

column(Id, date, Name, _Column, Opts) ->
    #{
        id => Id,
        header => [nkadmin_util:i18n(Name, Opts), #{ content => <<"serverFilter">> }],
        fillspace => <<"1">>,
        sort => <<"server">>,
        format => <<"
            function(value) {   // 'en-US', 'es-ES', etc.
                    return (new Date(value)).toLocaleString();
            }
        ">>
    };

column(Id, {icon, Icon}, _Name, _Column, _Opts) ->
    #{
        id => Id,
        header => <<"&nbsp;">>,
        width => 60,
        template => <<"
           <span style='cursor:pointer;' class='webix_icon #", ?BIN(Icon), "#'></span>
        ">>
    }.

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
on_click(Id, delete, _OnClick, _Opts, Acc) ->
    Acc#{
        Id => <<"
            function(e, id, node) {
                webix.confirm({
                    \"text\": \"This object will be deleted. <br/> Are you sure?\",
                    \"ok\": \"Yes\",
                    \"cancel\": \"Cancel\",
                    \"callback\": function(res) {
                        if(res) {
                            webix.$$(\"objectsData\").remove(id);
                        }
                    }
                });
            }
        ">>
    };

on_click(Id, disable, _OnClick, _Opts, Acc) ->
    Acc#{
        Id => <<"
            function(e, id, node) {
                webix.confirm({
                    \"text\": \"This object will be disabled. <br/> Are you sure?\",
                    \"ok\": \"Yes\",
                    \"cancel\": \"Cancel\",
                    \"callback\": function(res) {
                        if (res) {
                            var item = webix.$$(\"objectsData\").getItem(id);
                            item.enabled = false;
                            item.enabled_icon = \"fa-times\";
                            webix.$$(\"objectsData\").refresh(id);
                        }
                    }
                });
            }
        ">>
    };

on_click(Id, enable, _OnClick, _Opts, Acc) ->
    Acc#{
        Id => <<"
            function(e, id, node) {
                webix.confirm({
                    text: \"This object will be enabled. <br/> Are you sure?\",
                    ok: \"Yes\",
                    cancel: \"Cancel\",
                    callback: function(res) {
                        if (res) {
                            var item = webix.$$(\"objectsData\").getItem(id);
                            item.enabled = true;
                            item.enabled_icon = \"fa-check\";
                            webix.$$(\"objectsData\").refresh(id);
                        }
                    }
                });
            }
        ">>
    }.


%% @private
on_before_load(_Opts) ->
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
fake_delay() ->
    <<"
        function() {
            var grid = $$(\"objectsData\");
            grid.showProgress();
            webix.delay(function() {
                grid.hideProgress();
            }, null, null, 300);
        }
    ">>.