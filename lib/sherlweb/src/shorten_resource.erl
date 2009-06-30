%% @author Seth Falcon
%% @copyright 2009 Seth Falcon.
%% @doc shorten a long URL and return a simple HTML
%% page containing the new short URL.

-module(shorten_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    "/" ++ LongUrl = wrq:raw_path(ReqData),
    ShortUrl = make_short_url(LongUrl, ReqData),
    {["<html><body><a href=\"", ShortUrl, "\">", ShortUrl,
      "</a></body></html>"],
     ReqData, State}.

make_short_url(LongUrl, ReqData) ->
    {ok, Code} = sherl:encode(LongUrl),
    Host = wrq:get_req_header("host", ReqData),
    "http://" ++ Host ++ "/" ++ Code.

