-module(elna_sup).

-export([start_link/5]).

-export([init/1]).

-include_lib("chatterbox/include/http2.hrl").

-spec start_link(elna:id(), elna:elli_opts(), elna:chatterbox_opts(), elna:listen_opts(), elna:acceptor_opts())
                -> {ok, pid()}.
start_link(Id, ElliOpts, ChatterboxOpts, ListenOpts, AcceptorOpts) ->
    ElliOpts1 = [{accept_timeout, maps:get(accept_timeout, ElliOpts, 10000)},
                {request_timeout, maps:get(request_timeout, ElliOpts, 60000)},
                {header_timeout, maps:get(header_timeout, ElliOpts, 10000)},
                {body_timeout, maps:get(body_timeout, ElliOpts, 30000)},
                {max_body_size, maps:get(max_body_size, ElliOpts, 1024000)}],
    ChatterboxOpts1 = #settings{header_table_size=maps:get(header_table_size, ChatterboxOpts, 4096),
                                enable_push=maps:get(enable_push, ChatterboxOpts, 1),
                                max_concurrent_streams=maps:get(max_concurrent_streams, ChatterboxOpts, unlimited),
                                initial_window_size=maps:get(initial_window_size, ChatterboxOpts, 65535),
                                max_frame_size=maps:get(max_frame_size, ChatterboxOpts, 16384),
                                max_header_list_size=maps:get(max_header_list_size, ChatterboxOpts, unlimited)},
    supervisor:start_link(?MODULE, [Id, ElliOpts1, ChatterboxOpts1, ListenOpts, AcceptorOpts]).

init([Id, ElliOpts, ChatterboxOpts, ListenOpts, AcceptorOpts]) ->
    RestartStrategy = #{strategy => rest_for_one},
    Pool = #{id => {elna_pool, Id},
             start => {elna_pool, start_link, [Id, ElliOpts, ChatterboxOpts, ListenOpts]}},
    Socket = #{id => {elna_socket, Id},
               start => {elna_socket, start_link, [Id, ListenOpts, AcceptorOpts]}},
    {ok, {RestartStrategy, [Pool, Socket]}}.
