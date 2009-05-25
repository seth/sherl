-module(sherl_db).

-export([put_url/1, get_url/1]).

-include("../include/url.hrl").
-include("counter.hrl").

%% @spec put_url(string()) -> integer()
%% @doc Store a URL in the database and return a unique integer that
%% will be permanently associated with the URL.  If the URL is already
%% in the system, the already assigned unique integer is returned.
put_url(Url) ->
    ok.

%% @spec get_url(integer()) -> recUrl() | undefined
%% @type recUrl() = #url
%% @doc Return the URL associated with the specified integer ID.  The
%% atom undefined is returned if the specified URL is not found in the
%% database.
get_url(Code) ->
    ok.


