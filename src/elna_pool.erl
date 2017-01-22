-module(elna_pool).

-behaviour(acceptor_pool).

-export([start_link/4,
         accept_socket/3]).

-export([init/1]).

%% public api

start_link(Id, ElliOpts, ChatterboxOpts, ListenOpts) ->
    acceptor_pool:start_link({local, Id}, ?MODULE, [Id, ElliOpts, ChatterboxOpts, ListenOpts]).

accept_socket(Id, Socket, Acceptors) ->
    acceptor_pool:accept_socket(Id, Socket, Acceptors).

%% acceptor_pool api

init([Id, ElliOpts, ChatterboxOpts, ListenOpts]) ->
    application:set_env(chatterbox, stream_callback_mod, chatterbox_handler),
    {Transport, SslOpts} = case ListenOpts of
                               #{ssl := true,
                                 keyfile := KeyFile,
                                 certfile := CertFile} ->
                                   {ssl, [{keyfile, KeyFile},
                                          {certfile, CertFile},
                                          {honor_cipher_order, false},
                                          {versions, ['tlsv1.2']},
                                          {next_protocols_advertised, [<<"h2">>]}]};
                               false ->
                                   {gen_tcp, []}
                           end,
    Conn = #{id => {elna_conn, Id},
             start => {elna_conn, {Transport, ElliOpts, ChatterboxOpts, SslOpts}, []},
             grace => 5000},
    {ok, {#{}, [Conn]}}.
