%% @author Seth Falcon <seth@userprimary.net>
%% @copyright 2009 Seth Falcon.
-module(sherl_db).

-export([get_code/1, get_url/1, start/1, stop/0]).

-include("../include/url.hrl").
-record(counter, {id = 0, ver = 1}).

start([]) ->
    start([node()]);
start(Nodes) ->
    case is_fresh_startup() of
        true ->
            case mnesia:system_info(is_running) of
                yes ->
                    error_logger:info_report("stopping mnesia"),
                    mnesia:stop();
                _ -> pass
            end,
            mnesia:create_schema(Nodes),
            error_logger:info_report("mnesia schema created"),
            error_logger:info_report("starting mnesia"),
            mnesia:start(),
            mnesia:create_table(url, [{disc_copies, Nodes},
                                      {attributes, record_info(fields, url)},
                                      {index, [url]}]),
            mnesia:create_table(counter, [{disc_copies, Nodes},
                                          {attributes,
                                           record_info(fields, counter)}]),
            error_logger:info_report("mnesia tables created");
        {exists, Tables} ->
            ok = mnesia:wait_for_tables(Tables, 20000)
    end.

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
                case mnesia:read({url, Code}) of
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

%% @spec is_fresh_startup() -> true | false
%% @doc Returns true if mnesia has not been initialized with
%% the sherl schema.
%% Thanks to Dale Harvey for this function posted to
%% the erlang questions mailing list.
is_fresh_startup() ->
    Node = node(),
    case mnesia:system_info(tables) of
        [schema] -> true;
        Tbls ->
            case mnesia:table_info(schema, cookie) of
                {_, Node} -> {exists, Tbls};
                _                 -> true
            end
    end.
