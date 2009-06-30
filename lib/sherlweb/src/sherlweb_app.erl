%% @author Seth Falcon
%% @copyright 2009 Seth Falcon.

%% @doc Callbacks for the sherlweb application.

-module(sherlweb_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for sherlweb.
start(_Type, _StartArgs) ->
    sherlweb_deps:ensure(),
    sherlweb_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for sherlweb.
stop(_State) ->
    ok.
