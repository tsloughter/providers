-module(provider).

-export([]).

-callback init(any()) -> {ok, any()}.
-callback do(any()) ->  {ok, any()} | {error, string()} | {error, {module(), term()}}.
