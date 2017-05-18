%%%
%%% erpher_et: functions for event trace
%%% 
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author arkdro <arkdro@gmail.com>
%%% @since 2012-02-29 11:59
%%% @license MIT
%%% @doc functions for event trace
%%%

-module(erpher_et).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([
         trace_me/4, trace_me/5,
         viewer/1, viewer/2
        ]).

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc function that will be used in the event tracing.
%% The reason for defining it in its own module rather than using those
%% in the "et" module is that it gives some leeway when it comes to what
%% to trace. By letting each application have its own set of trace patterns,
%% you can make sure you only gets traces for the right kind of events
%% in a system.
%% @since 2012-02-29 12:07
%%
trace_me(_DetailLevel, _From, _To, _Label) ->
    trace_me(_DetailLevel, _From, _To, _Label, {}).

trace_me(_DetailLevel, _From, _To, _Label, _Contents) ->
    %erlang:display({?MODULE, ?LINE, _From, _To, _Label, _Contents}),
    hopefully_traced.

viewer(App) ->
    viewer(App, []).

viewer(App, ExtraOptions) ->
    Ecomet = [ecomet_server, ecomet_sockjs_handler, ecomet_conn_server, ecomet_conn_server_sjs, ecomet_auth_server, ecomet_data_msg, auth, sockjs],
    Ejobman = [ejobman_receiver_cmd, ejobman_group_handler, ejobman_group_handler_cmd],

    case App of
        _ -> Actors = Ecomet ++ Ejobman
    end,
    Options =
        [{event_order, event_ts},
            {scale, 2},
            {width, 1920},
            {height, 1080},
            {max_actors, 10},
            {detail_level, 100},
            {actors, Actors},
            {trace_pattern, {?MODULE, max}},
            {trace_global, true},
            {title, "Erpher tracer"} | ExtraOptions],
    et_viewer:start(Options).
