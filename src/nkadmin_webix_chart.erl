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

-module(nkadmin_webix_chart).
-export([chart/2, is_horizontal/1,
        parse_number_template/0, parse_number_template/1, parse_number_template/2, parse_color_template/1, parse_date_template/1, parse_date_template/2]).

-include("nkadmin.hrl").

%% ===================================================================
%% Types
%% ===================================================================

-type chart() ::
    #{
        value => binary(),
        color => hex_color(),
        tooltip => tooltip(),
        type => binary(),
        item => item(),
        line => line()
    }.

-type header() ::
    #{
        text => binary(),
        css => binary()
    }.

-type x_axis() ::
    #{
        template => binary(),
        title => binary(),
        lines => boolean(),
        lineColor => hex_color(),
        color => hex_color(),
        start => integer(),
        'end' => integer(),
        step => integer()
    }.

-type y_axis() ::
    #{
        template => binary(),
        title => binary(),
        lines => boolean(),
        lineColor => hex_color(),
        color => hex_color(),
        start => integer(),
        'end' => integer(),
        step => integer(),
        lineShape => arc | line,                            %% (radar)
        bg => hex_color()                                   %% (radar)
    }.

-type legend() ::
    #{
        width => integer(),
        height => integer(),
        layout => x | y,
        align => left | right | center,
        valign => top | bottom | middle,
        margin => integer(),
        padding => integer(),
        toggle => boolean(),
        template => binary(),                               %% (template AND marker) OR values (general or specific config)
        values => [value()],
        marker => marker()
    }.

-type value() ::
    #{
        text => binary(),
        color => hex_color(),
        markerType => square | round | item,
        toggle => boolean()
    }.

-type marker() ::
    #{
        type => square | round | item,
        width => integer(),
        height => integer(),
        radius => integer()
    }.

-type line() ::
    #{
        width => integer(),
        color => hex_color()
    }.

-type item() ::
    #{
        alpha => binary(),
        borderColor => hex_color(),
        borderWidth => integer(),
        color => hex_color(),
        radius => integer(),
        shadow => boolean(),
        type => r | d | s                                   %% (round, diamond, square)
    }.

-type padding() ::
    #{
        top => integer(),
        bottom => integer(),
        left => integer(),
        right => integer()
    }.

-type nk_function() ::
    #{
        nkParseFunction => binary()
    }.

-type template() ::
    #{
        template => binary()
    }.

-type hex_color() :: binary() | nk_function().

-type tooltip() :: boolean() | binary() | template().

-type chart_type() :: pie | pie3D | donut | line | spline | area | stackedArea | splineArea | bar | barH | stackedBar | stackedBarH | radar | scatter.

-type chart_spec() ::
    #{
        %% Common
        chart_id => binary(),                               %% Mandatory
        chart_type => chart_type(),                         %% Mandatory
        nk_charts => [binary()],                            %% Mandatory
        nk_interval_time => integer(),                      %% default: 5000
        min_height => integer(),
        min_width => integer(),
        base_domain => binary(),
        is_subchart => boolean(),
        header => header(),
        charts => [chart()],
        color => binary(),
        alpha => binary(),                                  %% default: 1.0
        border_color => hex_color(),
        borderless => boolean(),                            %% default: false
        data => map(),
        dynamic => boolean(),                               %% (line, spline, area and splineArea adds more data. Others will reload all data)
        event_radius => integer(),                          %% default: 0
        fix_overflow => boolean(),                          %% default: false
        label => binary(),
        legend => legend(),
        padding => padding(),
        remove_missed => boolean(),                         %% default: false
        tooltip => tooltip(),
        value => binary() | template(),
        %% Specific
        animate => boolean(),                               %% (line, spline, area, splineArea) default: true
        animate_duration => binary(),                       %% (line, spline, area, splineArea) default: 400 mSecs
        bar_width => integer(),                             %% (bar) default: 30 px
        border => boolean(),                                %% (bar) default: true
        cant => binary(),                                   %% (pie) default: 0.5
        cell_width => integer(),                            %% (line, spline, area, splineArea) default: 30
        disable_lines => boolean(),                         %% (radar) default: false
        fill => binary(),                                   %% (radar)
        gradient => boolean() | light | '3d' | rising | falling | binary(),     %% (bar, pie) default: light
        item => item(),                                     %% (line, radar, scatter)
        label_offset => integer(),                          %% (pie) default: 20
        line => line(),                                     %% (line, radar, scatter)
        line_color => hex_color(),                          %% (pie)
        offset => boolean(),                                %% (line, area, scatter) default: true
        origin => auto | integer(),                         %% (line, bar) default: auto
        pie_height => integer(),                            %% (pie3D)
        pie_inner_text => binary() | nk_function(),         %% (pie)
        radius => integer(),                                %% (bar) default: 0
        scale => linear | logarithmic,                      %% default: linear
        shadow => boolean(),                                %% (pie) default: true
        x => integer(),                                     %% (pie)
        y => integer(),                                     %% (pie)
        x_axis => x_axis(),                                 %% (line, bar, area, radar, scatter)
        x_value => binary(),                                %% (scatter)
        y_axis => y_axis(),                                 %% (line, bar, area, radar, scatter)
        y_value => binary()                                 %% (scatter)
    }.


%% ===================================================================
%% Public
%% ===================================================================

%% @doc
-spec chart(chart_spec(), nkadmin_session_obj:session()) ->
    map().

chart(#{chart_id:=ChartId}=Spec, Session) ->
    BodyId = case Spec of
        #{is_subchart:=true} ->
            ParentType = <<"clean">>,
            <<ChartId/binary, "_body">>;
        _ ->
            ParentType = <<"space">>,
            <<"body">>
    end,
    MinHeight = maps:get(min_height, Spec, 0),
    MinWidth = maps:get(min_width, Spec, 0),
    HeaderSpec = maps:get(header, Spec, #{}),
    HeaderText = maps:get(text, HeaderSpec, <<>>),
    Rows = case HeaderSpec of
        #{text:=_} ->
            HeaderCss = maps:get(css, HeaderSpec, <<>>),
            [
                #{
                    template => HeaderText,
                    height => 30,
                    padding => 0,
                    css => HeaderCss
                },
                make_charts(Spec, Session)
            ];
        #{} ->
            [
                make_charts(Spec, Session)
            ]
    end,
    #{
        %view => <<"scrollview">>,
        id => BodyId,
        %header => HeaderText,
        borderless => true,
        type => ParentType,
        css => <<"flex-tmp">>,
        %scroll => <<"xy">>,
        body => #{
            rows => [
                #{
                    type => ParentType,
                    minHeight => MinHeight,
                    minWidth => MinWidth,
                    rows => Rows
                }
            ]
        }
    }.

make_charts(#{chart_id:=ChartId, chart_type:=ChartType}=Spec, _Session) ->
    Charts = maps:get(charts, Spec, []),
    NKIntervalTime = maps:get(nk_interval_time, Spec, 5000),
    Alpha = maps:get(alpha, Spec, <<"1.0">>),
    Animate = maps:get(animate, Spec, true),
    AnimateDuration = maps:get(animate_duration, Spec, 400),
    BarWidth = maps:get(bar_width, Spec, 30),
    Border = maps:get(border, Spec, false),
    BorderColor = maps:get(border_color, Spec, <<>>),
    Borderless = maps:get(borderless, Spec, false),
    Cant = maps:get(cant, Spec, <<"0.5">>),
    Color = maps:get(color, Spec, <<>>),
    Data = maps:get(data, Spec, []),
    DisableLines = maps:get(disable_lines, Spec, false),
    Dynamic = maps:get(dynamic, Spec, false),
    CellWidth = case Dynamic of
        true ->
            maps:get(cell_width, Spec, 30);
        false ->
            maps:get(cell_width, Spec, <<>>)
    end,
    EventRadius = maps:get(event_radius, Spec, 10),
    Fill = maps:get(fill, Spec, <<>>),
    FixOverflow = maps:get(fix_overflow, Spec, false),
    Gradient = maps:get(gradient, Spec, false),
    Item = maps:get(item, Spec, <<>>),
    Label = maps:get(label, Spec, <<>>),
    LabelOffset = maps:get(label_offset, Spec, 20),
    Legend = maps:get(legend, Spec, <<>>),
    Line = maps:get(line, Spec, <<>>),
    LineColor = maps:get(line_color, Spec, <<>>),
    Offset = maps:get(offset, Spec, 0),
    Origin = maps:get(origin, Spec, auto),
    Padding = maps:get(padding, Spec, <<>>),
    PieHeight = maps:get(pie_height, Spec, 20),
    PieInnerText = maps:get(pie_inner_text, Spec, <<>>),
    Radius = maps:get(radius, Spec, 0),
    RemovedMissed = maps:get(remove_missed, Spec, false),
    Scale = maps:get(scale, Spec, linear),
    Shadow = maps:get(shadow, Spec, true),
    Tooltip = maps:get(tooltip, Spec, <<>>),
    Value = maps:get(value, Spec, <<>>),
    X = maps:get(x, Spec, <<>>),
    Y = maps:get(y, Spec, <<>>),
    XAxis = maps:get(x_axis, Spec, <<>>),
    YAxis = maps:get(y_axis, Spec, <<>>),
    XValue = maps:get(x_value, Spec, <<>>),
    YValue = maps:get(y_value, Spec, <<>>),
    NKIntervalFunction = case Dynamic of
        false ->
            <<"
                function(chartId) {
                }
            ">>;
        true ->
            <<"
                function(chartId) {
                    //var chart = $$('", ChartId/binary, "');
                    var chart = $$(chartId);
                    //console.log('nkIntervalFunction', chartId, chart);
                    if (chart) {
                        if (chart.nkTimer) {
                            // Clear this inteval
                            //console.log('nkIntervalFunction clearInterval', chart);
                            clearInterval(chart.nkTimer);
                            chart.nkTimer = null;
                        }
                        chart.load('wsChartProxy->', function(text, data, http_request) {
                            // Data loaded
                            //var chart = $$('", ChartId/binary, "');
                            var chart = $$(chartId);
                            if (chart && chart.config && chart.config.dynamic) {
                                // Set next interval
                                //console.log('nkIntervalFunction setNextInterval');
                                chart.nkTimer = setInterval(function() { chart.config.nkIntervalFunction(chart) }, chart.config.nkIntervalTime);
                            }
                        });
                    }
                }
            ">>
    end,
    JSON = #{
        % Common configuration
        id => ChartId,
        view => <<"chart">>,
        type => ChartType,
        nkCount => 0,
        nkIntervalTime => NKIntervalTime,
        nkIntervalFunction => #{
            nkParseFunction => NKIntervalFunction
        },
        url => <<"wsChartProxy->">>,
        ready => #{
            nkParseFunction => <<"
                function() {
                    // Set interval
                    //var chart = $$('", ChartId/binary, "');
                    var chart = $$(this.config.id);
                    var chartId = this.config.id;
                    if (this !== chart) {
                        if (chart) {
                            chartId = chart.config.id;
                        }
                        chart = this;
                    }
                    //console.log('onReady test'+'ing', this.config.id, this === $$(this.config.id), chartId);
                    if (chart) {
                        if (chart.config.dynamic) {
                            // Setting first interval timer
                            chart.nkTimer = setInterval(function() { chart.config.nkIntervalFunction(chart.config.id) }, chart.config.nkIntervalTime);
                        } else {
                            chart.config.nkIntervalFunction(chart.config.id);
                        }
                        // And a function to clear that interval on destruct
                        chart.attachEvent('onDestruct', function() {
                            //console.log('onDestruct test'+'ing', this.config.id, this, this.nkTimer);
                            if (this.nkTimer) {
                                clearInterval(this.nkTimer);
                                this.nkTimer = null;
                                //console.log('timer cleared', this.nkTimer);
                            }
                        })
                    }
                }
            ">>
        },
        % Specific properties
        alpha => Alpha,
        animate => Animate,
        animateDuration => AnimateDuration,
        barWidth => BarWidth,
        border => Border,
        borderColor => BorderColor,
        borderless => Borderless,
        cant => Cant,
        cellWidth => CellWidth,
        color => Color,
        data => Data,
        disableLines => DisableLines,
        dynamic => Dynamic,
        eventRadius => EventRadius,
        fill => Fill,
        fixOverflow => FixOverflow,
        gradient => Gradient,
        item => Item,
        label => Label,
        labelOffset => LabelOffset,
        legend => Legend,
        line => Line,
        lineColor => LineColor,
        offset => Offset,
        origin => Origin,
        padding => Padding,
        pieHeight => PieHeight,
        pieInnerText => PieInnerText,
        radius => Radius,
        removedMissed => RemovedMissed,
        scale => Scale,
        shadow => Shadow,
        tooltip => Tooltip,
        value => Value,
        x => X,
        y => Y,
        xAxis => XAxis,
        yAxis => YAxis,
        xValue => XValue,
        yValue => YValue
    },
    JSON2 = case add_series(ChartType, Charts, []) of
        [] ->
            JSON;
        Series ->
            JSON#{series => Series}
    end,
    filter_properties_by_chart_type(ChartType, JSON2).


%% @doc
-spec is_horizontal(chart_type()) ->
    boolean().

is_horizontal(<<"barH">>) ->
    true;
is_horizontal(<<"stackedBarH">>) ->
    true;
is_horizontal(_ChartType) ->
    false.

%% @doc
-spec parse_number_template() ->
    map().

parse_number_template() ->
    parse_number_template(<<>>).


%% @doc
-spec parse_number_template(binary()) ->
    map().

parse_number_template(Default) ->
    #{
        nkParseFunction => <<"
            function(obj) {
                if (obj >= 1000000)  {
                    return (obj/1000000 + 'M');
                } else if (obj >= 1000)  {
                    return (obj/1000 + 'K');
                } else if (obj == 0) {
                    return '", Default/binary, "'
                }
                return obj;
            }
        ">>
    }.


%% @doc
-spec parse_number_template(binary(), binary()) ->
    map().
    
parse_number_template(Value, Default) ->
    #{
        nkParseFunction => <<"
            function(obj) {
                var field = '", Value/binary, "';
                var value;
                if (obj && obj[field]) {
                    value = obj[field];
                    if (value >= 1000000) {
                        return value/1000000 + 'M';
                    } else if (value >= 1000) {
                        return value/1000 + 'K';
                    } else if (value == 0) {
                        return '", Default/binary, "'
                    } else {
                        return value;
                    }
                }
                return obj;
            }
        ">>
    }.
    

%% @doc
-spec parse_color_template(binary()) ->
    map().
        
parse_color_template(Value) ->
    #{
        nkParseFunction => <<"
            function(obj) {
                var field = '", Value/binary, "';
                if (obj && obj[field]) {
                    return obj[field];
                }
                return 'black';
            }
        ">>
    }.


%% @doc
-spec parse_date_template(binary()) ->
    map().

parse_date_template(Value) ->
    parse_date_template(Value, <<"{}">>).

%% @doc
-spec parse_date_template(binary(), map()) ->
    map().
            
parse_date_template(Value, Opts) ->
    #{
        nkParseFunction => <<"
            function(obj) {
                var field = '", Value/binary, "';
                if (obj && obj[field]) {
                    return (new Date(obj[field])).toLocaleString('en-US', ", Opts/binary, ");
                }
                return obj;
            }
        ">>
    }.    


add_series(<<"pie">>, _, _) ->
    [];

add_series(<<"pie3D">>, _, _) ->
    [];

add_series(<<"donut">>, _, _) ->
    [];

add_series(_ChartType, [], Series) ->
    lists:reverse(Series);

add_series(ChartType, [#{value:=Value}=Chart | Charts], Series) ->
    Color = maps:get(color, Chart, <<>>),
    Type = maps:get(type, Chart, <<>>),
    Tooltip = maps:get(tooltip, Chart, <<>>),
    Item = maps:get(item, Chart, <<>>),
    Line = maps:get(line, Chart, <<>>),
    S = case Type of
        <<>> ->
            filter_unused_properties(#{value => Value, color => Color, tooltip => Tooltip, item => Item, line => Line});
        _ ->
            case are_compatible_charts(ChartType, Type) of
                true ->
                    Chart;
                false ->
                    filter_unused_properties(#{value => Value, color => Color, tooltip => Tooltip})
            end
    end,
    add_series(ChartType, Charts, [S|Series]);

add_series(ChartType, [_|Charts], Series) ->
    add_series(ChartType, Charts, Series).


filter_properties_by_chart_type(Type, JSON) ->
    % Get useful properties by chart type (common and specific)
    Specific = get_properties_by_chart_type(Type),
    WhiteList = [id, view, type, nkCount, nkIntervalTime, nkIntervalFunction, url, ready, series, alpha, borderColor, borderless, color, data, dynamic, eventRadius, fixOverflow, label, legend, padding, removedMissed, tooltip, value | Specific],
    % Filter useful properties
    JSON2 = maps:with(WhiteList, JSON),
    % Filter unused properties
    filter_unused_properties(JSON2).



get_properties_by_chart_type(<<"line">>) ->
    [animate, animateDuration, cellWidth, item, line, offset, origin, scale, series, xAxis, yAxis];

get_properties_by_chart_type(<<"spline">>) ->
    get_properties_by_chart_type(<<"line">>);

get_properties_by_chart_type(<<"area">>) ->
    get_properties_by_chart_type(<<"line">>);

get_properties_by_chart_type(<<"stackedArea">>) ->
    get_properties_by_chart_type(<<"line">>);

get_properties_by_chart_type(<<"splineArea">>) ->
    get_properties_by_chart_type(<<"line">>);

get_properties_by_chart_type(<<"bar">>) ->
    [barWidth, border, gradient, origin, radius, scale, series, xAxis, yAxis];

get_properties_by_chart_type(<<"barH">>) ->
    get_properties_by_chart_type(<<"bar">>);

get_properties_by_chart_type(<<"stackedBar">>) ->
    get_properties_by_chart_type(<<"bar">>);

get_properties_by_chart_type(<<"stackedBarH">>) ->
    get_properties_by_chart_type(<<"bar">>);

get_properties_by_chart_type(<<"pie">>) ->
    [cant, gradient, labelOffset, lineColor, pieInnerText, shadow, x, y];

get_properties_by_chart_type(<<"pie3D">>) ->
    [pieHeight | get_properties_by_chart_type(<<"pie">>)];

get_properties_by_chart_type(<<"donut">>) ->
    get_properties_by_chart_type(<<"pie">>);

get_properties_by_chart_type(<<"radar">>) ->
    [disableLines, fill, item, line, scale, series, xAxis, yAxis];

get_properties_by_chart_type(<<"scatter">>) ->
    [fill, item, line, origin, scale, series, xAxis, xValue, yAxis, yValue].



%% @private
has_series(Main) ->
    is_line(Main) orelse is_area(Main) orelse is_radar(Main) orelse is_scatter(Main).


%% @private
are_compatible_charts(Main, Other) ->
    case is_stacked(Main) orelse is_stacked(Other) orelse is_radar(Main) orelse is_radar(Other) orelse is_scatter(Main) orelse is_scatter(Other) orelse Main == <<"barH">> of
        true ->
            false;
        false when Other == <<"barH">> ->
            false;
        false ->
            has_series(Main) andalso has_series(Other)
    end.


%% @private
are_same_chart_type(Main, Other) ->
    (is_pie(Main) andalso is_pie(Other))
    orelse
    (is_line(Main) andalso is_line(Other))
    orelse
    (is_area(Main) andalso is_area(Other))
    orelse
    (is_radar(Main) andalso is_radar(Other))
    orelse
    (is_scatter(Main) andalso is_scatter(Other)).


%% @private
is_pie(<<"pie">>) ->
    true;
is_pie(<<"pie3D">>) ->
    true;
is_pie(<<"donut">>) ->
    true;
is_pie(_) ->
    false.

%% @private
is_line(<<"line">>) ->
    true;
is_line(<<"spline">>) ->
    true;
is_line(_) ->
    false.

%% @private
is_area(<<"area">>) ->
    true;
is_area(<<"stackedArea">>) ->
    true;
is_area(<<"splineArea">>) ->
    true;
is_area(_) ->
    false.

%% @private
is_bar(<<"bar">>) ->
    true;
is_bar(<<"barH">>) ->
    true;
is_bar(<<"stackedBar">>) ->
    true;
is_bar(<<"stackedBarH">>) ->
    true;
is_bar(_) ->
    false.

%% @private
is_radar(<<"radar">>) ->
    true;
is_radar(_) ->
    false.

%% @private
is_scatter(<<"scatter">>) ->
    true;
is_scatter(_) ->
    false.


%% @private
is_stacked(<<"stackedArea">>) ->
    true;
is_stacked(<<"stackedBar">>) ->
    true;
is_stacked(<<"stackedBarH">>) ->
    true;
is_stacked(_) ->
    false.


%% @private
filter_unused_properties(Map) ->
    maps:filter(fun(_,V) -> V =/= <<>> end, Map).


%% @private
append_id(ChartId, Id) ->
    <<ChartId/binary, "__", Id/binary>>.


i18n(<<"&nbsp;">>, _Session) -> <<>>;
i18n(Key, Session) -> nkadmin_util:i18n(Key, Session).


%% @private
to_bin(Term) when is_binary(Term) -> Term;
to_bin(Term) -> nklib_util:to_binary(Term).
