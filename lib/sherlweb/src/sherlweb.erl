%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(sherlweb).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start() -> ok
%% @doc Start the sherlweb server.
start() ->
    sherlweb_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    ensure_started(mnesia),
    ensure_started(sherl),
    application:start(sherlweb).

%% @spec stop() -> ok
%% @doc Stop the sherlweb server.
stop() ->
    Res = application:stop(sherlweb),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
