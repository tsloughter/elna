elna
=====

Erlang HTTP 1.1 and 2 server using Elli and Chatterbox

Run
-----

```
$ rebar3 shell
> elna:start_link(#{id => example,
                    listen_opts => #{port => 8080,
                                     ssl => true,
                                     certfile => filename:join(code:priv_dir(elna), "localhost.crt"),
                                     keyfile => filename:join(code:priv_dir(elna), "localhost.key")}}).
```

```
$ h2c connect localhost:8080
$ h2c get /
Hello from chatterbox!
```
