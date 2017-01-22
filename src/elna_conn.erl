-module(elna_conn).

-behaviour(acceptor).

-export([acceptor_init/3,
         acceptor_continue/3,
         acceptor_terminate/2]).

-define(ELLI_CALLBACK, {elli_callback, []}).

acceptor_init(_, LSocket, {Transport, ElliOptions, ChatterboxOpts, SslOpts}) ->
    % monitor listen socket to gracefully close when it closes
    MRef = monitor(port, LSocket),
    {ok, {Transport, MRef, ElliOptions, ChatterboxOpts, SslOpts}}.

acceptor_continue(_PeerName, Socket, {ssl, _MRef, ElliOptions, ChatterboxOpts, SslOpts}) ->
    {ok, AcceptSocket} = ssl:ssl_accept(Socket, SslOpts),
    case ssl:negotiated_protocol(AcceptSocket) of
        {ok, <<"h2">>} ->
            h2_connection:become({ssl, AcceptSocket}, ChatterboxOpts);
        _ ->
            elli_http:keepalive_loop({ssl, AcceptSocket}, ElliOptions, ?ELLI_CALLBACK)
    end;
acceptor_continue(_PeerName, Socket, {gen_tcp, _MRef, ElliOptions, _ChatterboxOpts, _SslOpts}) ->
    elli_http:keepalive_loop({plain, Socket}, ElliOptions, ?ELLI_CALLBACK).

acceptor_terminate(Reason, _) ->
    % Something went wrong. Either the acceptor_pool is terminating or the
    % accept failed.
    exit(Reason).
