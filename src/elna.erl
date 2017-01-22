-module(elna).

%% API exports
-export([start_link/0,
         start_link/5]).

-export_types([id/0,
               elli_opts/0,
               listen_opts/0,
               acceptor_opts/0]).

-type id() :: atom().
-type elli_opts() :: #{accept_timeout  => integer(),
                       request_timeout => integer(),
                       header_timeout  => integer(),
                       body_timeout   => integer(),
                       max_body_size   => integer()}.
-type chatterbox_opts() :: #{header_table_size => integer(),
                             enable_push => 0 | 1,
                             max_concurrent_streams => integer() | unlimited,
                             initial_window_size => integer(),
                             max_frame_size => integer(),
                             max_header_list_size => integer() | unlimited}.
-type listen_opts() :: #{ip => inet:ip_address(),
                       port => integer(),
                       socket_options => [gen_tcp:option()],
                       ssl => boolean(),
                       certfile => ssl:path(),
                       keyfile => ssl:path()}.
-type acceptor_opts() :: #{pool_size => integer()}.

start_link() ->
    AcceptorOpts = #{acceptor_pool_size => 10},
    ListenOpts = #{ssl => true,
                   certfile => filename:join(code:priv_dir(elna), "localhost.crt"),
                   keyfile => filename:join(code:priv_dir(elna), "localhost.key"),

                   ip => {0,0,0,0},
                   port => 8080,

                   socket_options => [{reuseaddr, true},
                                      {nodelay, true},
                                      {reuseaddr, true},
                                      {backlog, 32768},
                                      {keepalive, true}]},
    start_link(?MODULE, #{}, #{}, ListenOpts, AcceptorOpts).

-spec start_link(elna:id(), elna:elli_opts(), chatterbox_opts(), elna:listen_opts(), elna:acceptor_opts())
                -> {ok, pid()}.
start_link(Id, ElliOpts, ChatterboxOpts, ListenOpts, AcceptorOpts) ->
    elna_sup:start_link(Id, ElliOpts, ChatterboxOpts, ListenOpts, AcceptorOpts).
