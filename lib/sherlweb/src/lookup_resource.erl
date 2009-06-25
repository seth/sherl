%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(lookup_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {ok, LongUrl} = sherl:decode(wrq:disp_path(ReqData)),
    {["<html><body><a href=\"", LongUrl, "\">", LongUrl,
      "</a></body></html>"],
     ReqData, State}.


