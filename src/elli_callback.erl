-module(elli_callback).

-export([handle/2,
         handle_event/3]).

-include_lib("elli/include/elli.hrl").

handle(_Req, _) ->
    {ok, [], <<"Hello from elli!">>}.

handle_event(_, _, _) -> ok.
