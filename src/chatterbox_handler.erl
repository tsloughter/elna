-module(chatterbox_handler).

-behaviour(h2_stream).

-export([init/2,
         on_receive_request_headers/2,
         on_send_push_promise/2,
         on_receive_request_data/2,
         on_request_end_stream/1]).

-include_lib("chatterbox/include/http2.hrl").

-record(cb_static, {
          req_headers=[],
          connection_pid :: pid(),
          stream_id :: stream_id()
         }).

init(ConnPid, StreamId) ->
    {ok, #cb_static{connection_pid=ConnPid,
                    stream_id=StreamId}}.

on_receive_request_headers(Headers, State) ->
    {ok, State#cb_static{req_headers=Headers}}.

on_send_push_promise(Headers, State) ->
    {ok, State#cb_static{req_headers=Headers}}.

on_receive_request_data(_Bin, State)->
    {ok, State}.

on_request_end_stream(State=#cb_static{connection_pid=ConnPid,
                                       stream_id=StreamId}) ->
    ResponseHeaders = [{<<":status">>,<<"200">>}],
    h2_connection:send_headers(ConnPid, StreamId, ResponseHeaders),
    h2_connection:send_body(ConnPid, StreamId, <<"Hello from chatterbox!">>),
    {ok, State}.
