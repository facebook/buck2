%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(io_buffer).
-moduledoc """
An IOBuffer is an input only IOServer
(see https://www.erlang.org/doc/apps/stdlib/io_protocol.html)
that acts as a buffer, storing IO inputs in a queue
that one can empty using the flush method.
It is designed to work alongside a "group leader", capturing
IOEvents directed at it. This capture can be started/stopped
using appropriate methods.
""".
-compile(warn_missing_spec_all).

-record(state, {buffer, process, group_leader, capture, pass_through, max_elements, max_length}).

-define(ERROR_MSG, "This is a write_only IO server").

-export([start_link/0, start_link/1, flush/1, stop/1, start_capture/1, stop_capture/1]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

% For testing
-export([trim_line/2]).

-import(common_util, [unicode_characters_to_list/1]).

% Max number of entries kept in the buffer.
% After that, the buffer will be truncated in the middle, see
% bounded_buffer.
-define(MAX_LINES, 20000).

% Maximum num of characters stored in a single line of the buffer.
-define(MAX_LENGTH, 40000).

%% Public API

-type start_args() :: #{
    passthrough => boolean(),
    max_elements => pos_integer(),
    max_length => pos_integer()
}.
-export_type([start_args/0]).

-doc """
Starts an linked IO server.
""".
-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link(#{passthrough => true, max_elements => ?MAX_LINES, max_length => ?MAX_LENGTH}).

-spec start_link(Args) -> gen_server:start_ret() when
    Args :: start_args().
start_link(Args) ->
    gen_server:start_link(?MODULE, {self(), group_leader(), Args}, []).

-doc """
Empties the buffer and retrieves its content.
""".
-spec flush(IoBuffer) -> {string(), boolean()} when
    IoBuffer :: gen_server:server_ref().
flush(IoBuffer) ->
    do_call(IoBuffer, flush).

-doc """
Starts caturing IOEvents and redirecting them to its queue.
""".
-spec start_capture(IoBuffer) -> ok when
    IoBuffer :: gen_server:server_ref().
start_capture(IoBuffer) ->
    do_call(IoBuffer, start_capture).

-doc """
Stops capturing IOEvents, letting them flow to their initial group leader.
""".
-spec stop_capture(IoBuffer) -> ok when
    IoBuffer :: gen_server:server_ref().
stop_capture(IoBuffer) ->
    do_call(IoBuffer, stop_capture).

-spec do_call(gen_server:server_ref(), flush | start_capture | stop_capture) -> dynamic().
do_call(IoBuffer, Request) ->
    gen_server:call(IoBuffer, Request).

-doc """
Stop the IoBuffer
""".
-spec stop(IoBuffer) -> ok when
    IoBuffer :: gen_server:server_ref().
stop(IoBuffer) ->
    gen_server:stop(IoBuffer).

-spec init({Process, GroupLeader, Args}) -> {ok, #state{}} when
    Process :: pid(),
    GroupLeader :: pid(),
    Args :: start_args().
init({Process, GroupLeader, Args}) ->
    #{passthrough := PassThrough, max_elements := MaxElements, max_length := MaxLength} = Args,
    group_leader(self(), Process),
    {ok, #state{
        process = Process,
        group_leader = GroupLeader,
        buffer = bounded_buffer:new(MaxElements),
        capture = false,
        pass_through = PassThrough,
        max_elements = MaxElements,
        max_length = MaxLength
    }}.

-spec handle_call
    (flush, gen_server:from(), #state{}) -> {reply, {list(), boolean()}, #state{}};
    (start_capture, gen_server:from(), #state{}) -> {reply, ok, #state{}};
    (stop_capture, gen_server:from(), #state{}) -> {reply, ok, #state{}}.
handle_call(flush, _From, State = #state{buffer = Buffer, max_elements = MaxElements}) ->
    NewState = State#state{buffer = bounded_buffer:new(MaxElements)},
    {Elements, Truncated} = bounded_buffer:get_elements(Buffer),
    {reply, {unicode_characters_to_list(Elements), Truncated}, NewState};
handle_call(start_capture, _From, State = #state{}) ->
    {reply, ok, State#state{capture = true}};
handle_call(stop_capture, _From, #state{group_leader = GroupLeader, buffer = Buffer} = State) ->
    {Elements, _Truncated} = bounded_buffer:get_elements(Buffer),
    lists:foreach(fun(Chars) -> io:put_chars(GroupLeader, Chars) end, Elements),
    {reply, ok, State#state{capture = false}};
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info({io_request, From, ReplyAs, Request}, #state{}) -> {noreply, #state{}} when
    From :: erlang:send_destination(),
    ReplyAs :: term(),
    Request :: request().
handle_info(
    {io_request, From, ReplyAs, Request},
    #state{capture = Capture, pass_through = PassThrough, group_leader = GroupLeader} = State
) ->
    {Reply, State1} =
        case Capture of
            true ->
                request(Request, State);
            _ ->
                {ok, State}
        end,
    case not Capture orelse PassThrough of
        true ->
            % The GroupLeader, acting as a IoServer, will provide a reply.
            GroupLeader ! {io_request, From, ReplyAs, Request};
        _ ->
            % Otherwise we have to use the previously computed reply.
            From ! {io_reply, ReplyAs, Reply}
    end,
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

-type request() ::
    % Output Requests
    {put_chars, Encoding :: unicode:encoding(),
        Characters :: unicode:latin1_chardata() | unicode:chardata() | unicode:external_chardata()}
    | {put_chars, Encoding :: unicode:encoding(), Module :: module(), Function :: atom(), Args :: [term()]}
    % Input Requests
    | {get_until, Encoding :: unicode:encoding(), Prompt :: term(), Module :: module(), Function :: atom(),
        ExtraArgs :: [term()]}
    | {get_chars, Encoding :: unicode:encoding(), Prompt :: term(), N :: integer()}
    | {get_line, Encoding :: unicode:encoding(), Prompt :: term()}
    % Other Requests
    | getopts
    | {setopts, Opts :: term()}
    | {get_geometry, Geometry :: term()}
    | {requests, [request()]}.

-type 'io:getopt'() :: {'terminal' | 'stdin' | 'stdout' | 'stderr', boolean()} | 'io:option'().
-type 'io:option'() :: {atom(), term()}.

-spec request(request(), #state{}) -> {ok | ['io:getopt'()] | {error, term()}, #state{}}.
request({put_chars, Encoding, Chars}, #state{buffer = Buffer, max_length = MaxLength} = State) ->
    Line =
        case unicode:characters_to_binary(Chars, Encoding) of
            Error = {'incomplete', _List, _Binary} -> error(Error);
            Error = {'error', _List, _Rest} -> error(Error);
            String -> String
        end,
    Trimmed = trim_line(Line, MaxLength),
    NewBuffer = bounded_buffer:put(Buffer, Trimmed),
    {ok, State#state{buffer = NewBuffer}};
request({put_chars, Encoding, Module, Function, Args}, State) ->
    try
        request({put_chars, Encoding, erlang:apply(Module, Function, Args)}, State)
    catch
        _:_ ->
            {{error, Function}, State}
    end;
request({get_until, _Encoding, _Prompt, _M, _F, _As}, State) ->
    {{error, ?ERROR_MSG}, State};
request({get_chars, _Encoding, _Prompt, _N}, State) ->
    {{error, ?ERROR_MSG}, State};
request({get_line, _Encoding, _Prompt}, State) ->
    {{error, ?ERROR_MSG}, State};
request({requests, Reqs}, State) ->
    multi_request(Reqs, {ok, State});
request(getopts, State = #state{group_leader = Gl}) ->
    {io:getopts(Gl), State};
request({setopts, _Opts}, State) ->
    {ok, State};
request({get_geometry, _Geometry}, State) ->
    {{error, enotsup}, State};
request(Other, State) ->
    {{error, {wrong_request, Other}}, State}.

-spec multi_request(Requests, {LatestReply, State}) -> {LatestReply, State} when
    Requests :: [request()],
    LatestReply :: ok | ['io:getopt'()] | {error, term()},
    State :: #state{}.
multi_request([], {LatestReply, State}) ->
    {LatestReply, State};
multi_request([_ | _], {{error, Error}, State}) ->
    {{error, Error}, State};
multi_request([R | Rs], {_Reply, State}) ->
    multi_request(Rs, request(R, State)).

-spec terminate(Reason, State) -> ok when
    Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}.
terminate(_Reason, #state{process = Process, group_leader = GroupLeader} = _State) ->
    group_leader(GroupLeader, Process),
    ok.

-spec trim_line(binary(), integer()) -> binary().
trim_line(Line, Length) when byte_size(Line) > Length ->
    TruncLine = string:slice(Line, 0, Length),
    <<TruncLine/binary, "... line truncated"/utf8>>;
trim_line(Line, _Length) ->
    Line.
