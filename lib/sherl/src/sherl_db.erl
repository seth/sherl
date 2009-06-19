-module(sherl_db).

-export([get_code/1, get_url/1, start/1, stop/0]).

-include("../include/url.hrl").
-include("counter.hrl").
-include_lib("stdlib/include/qlc.hrl").

start([]) ->
    start([node()]);
start(Nodes) ->
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(url, [{disc_copies, Nodes},
                              {attributes, record_info(fields, url)},
                              {index, [url]}]),
    mnesia:create_table(counter, [{disc_copies, Nodes},
                                  {attributes, record_info(fields, counter)}]),
    mnesia:wait_for_tables([url, counter], 20000).

stop() ->
    mnesia:stop().

%% @spec get_code(string()) -> recUrl()
%% @type recUrl() = #url
%% @doc Store a URL in the database and return a unique integer that
%% will be permanently associated with the URL.  If the URL is already
%% in the system, the already assigned unique integer is returned.
get_code(Url) ->
    F = fun() ->
                mnesia:lock({table, url}, write),
                case mnesia:index_read(url, Url, #url.url) of
                    [] ->
                        New = #url{url = Url, code = next_int(), created = now()},
                        mnesia:write(New),
                        New;
                    [Found] ->
                        Found
                end
        end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% @spec get_url(integer()) -> recUrl() | undefined
%% @type recUrl() = #url
%% @doc Return the URL associated with the specified integer ID.  The
%% atom undefined is returned if the specified URL is not found in the
%% database.
get_url(Code) ->
    F = fun() ->
                Q = qlc:q([X || X <- mnesia:table(url), X#url.code =:= Code ]),
                Recs = qlc:e(Q),
                case Recs of
                    [] ->
                        undefined;
                    [Rec] ->
                        Rec
                end
        end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

%% @spec next_int() -> integer()
%% @doc This is the integer sequence used to uniquely identify URLs
next_int() ->
    mnesia:dirty_update_counter(counter, id, 1).
