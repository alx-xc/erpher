%%%
%%% erpher_rt_stat_web_server: runtime statistics web server
%%%
%%% Copyright (c) 2014 Megaplan Ltd. (Russia)
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
%%% @author alx <alxphp@gmail.com>
%%% @since 2014-03-19 16:48
%%% @license MIT
%%% @doc Run statistics web server
%%% 

-module(erpher_rt_stat_web_server).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("estat.hrl").

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(_) ->
    mpln_p_debug:ir({?MODULE, ?LINE, 'init...'}),
    St = erpher_rt_stat_conf:get_web_config(),
    St_new = prepare_web(St),
    mpln_p_debug:ir({?MODULE, ?LINE, 'init done'}),
    {ok, St_new}.

%------------------------------------------------------------------------------
-spec handle_call(any(), any(), #est{}) ->
                         {stop, normal, ok, #est{}}
                             | {reply, any(), #est{}}.
%%
%% Handling call messages
%%
handle_call(_Req, _From, St) ->
    mpln_p_debug:er({?MODULE, ?LINE, call_other, _Req}),
    {reply, {error, unknown_request}, St}.

%------------------------------------------------------------------------------
-spec handle_cast(any(), #est{}) -> any().
%%
%% Handling cast messages
%%
handle_cast(_Req, St) ->
    mpln_p_debug:er({?MODULE, ?LINE, cast_other, _Req}),
    {noreply, St}.

%------------------------------------------------------------------------------
terminate(Reason, State) ->
    mpln_p_debug:er({?MODULE, ?LINE, terminate, Reason}),
    ok.

%------------------------------------------------------------------------------
-spec handle_info(any(), #est{}) -> any().
%%
%% Handling all non call/cast messages
%%
handle_info(_Req, State) ->
    mpln_p_debug:er({?MODULE, ?LINE, info_other, _Req}),
    {noreply, State}.

%------------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
-spec start() -> any().
%%
%% @doc starts receiver gen_server
%%
start() ->
    start_link().

%%-----------------------------------------------------------------------------
%%
%% @doc starts receiver gen_server with pre-defined config
%%
-spec start_link() -> any().

start_link() ->
    start_link(?CONF).

%%
%% @doc starts receiver gen_server with given config
%%
-spec start_link(string()) -> any().

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%-----------------------------------------------------------------------------
%%
%% @doc stops receiver gen_server
%%
-spec stop() -> any().

stop() ->
    gen_server:call(?MODULE, stop).


%%
%% @doc prepare web server which is used to serve worker monitoring page only
%%
-spec prepare_web(#est_web{}) -> #est_web{}.

prepare_web(#est_web{port = false} = St) ->
    St;

prepare_web(#est_web{port = Port} = St) ->
    Dispatch = cowboy_router:compile([
        {'_', [{'_', erpher_rt_stat_web_handler, []}]}
    ]),

    {ok, _Listener} = cowboy:start_http(?MODULE, 100,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    St.

%%-----------------------------------------------------------------------------
