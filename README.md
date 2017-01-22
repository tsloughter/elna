elna
=====

Erlang HTTP 1.1 and 2 server using Elli and Chatterbox

Run
-----

```
$ rebar3 shell
> elna:start_link().
```

```
$ h2c connect localhost:8080
$ h2c get /
Hello from chatterbox!
```
