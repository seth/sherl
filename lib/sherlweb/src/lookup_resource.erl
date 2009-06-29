%% @author Seth Falcon <seth@userprimary.net>
%% @copyright 2009 Seth Falcon.
%% @doc The sherl lookup resource expects sherl codes and returns
%% either a 301 redirect or a 404, if the code was not found.

-module(lookup_resource).
-export([init/1, moved_permanently/2, previously_existed/2,
         resource_exists/2]).

-include_lib("webmachine/include/webmachine.hrl").

-record(sherl, {long_url = undefined}).

init([]) -> {ok, #sherl{}}.

previously_existed(ReqData, State) ->
    case State#sherl.long_url of
        undefined ->
            {false, ReqData, State};
        _ ->
            {true, ReqData, State}
    end.

moved_permanently(ReqData, State) ->
    case State#sherl.long_url of
        undefined ->
            {false, ReqData, State};
        LongUrl ->
            {{true, LongUrl}, ReqData, State}
    end.

resource_exists(ReqData, State) ->
    {Status, LongUrl} = sherl:decode(wrq:disp_path(ReqData)),
    case Status of
        ok -> {false, ReqData, State#sherl{long_url = LongUrl}};
        _ ->
            Msg = [<<"<html><title>unknown sherl</title>">>,
                   <<"<body>unknown sherl: ">>,
                   list_to_binary(wrq:disp_path(ReqData)),
                   <<"</body></html>\n">>],
            NotFoundData = wrq:set_resp_body(Msg, ReqData),
            {false, NotFoundData, State}
    end.
