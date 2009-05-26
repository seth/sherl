-module(sherl_db_SUITE).

-compile(export_all).

-include("../include/url.hrl").
-include_lib("ct.hrl").

%%--------------------------------------------------------------------
%% Function: suite() -> DefaultData
%% DefaultData: [tuple()]
%% Description: Require variables and set default values for the suite
%%--------------------------------------------------------------------
suite() -> [{timetrap,{seconds,10}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config) -> Config
%% Config: [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Initiation before the whole suite
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config) -> _
%% Config: [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Description: Cleanup after the whole suite
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: all() -> TestCases
%% TestCases: [Case]
%% Case: atom()
%%   Name of a test case.
%% Description: Returns a list of all test cases in this test suite
%%--------------------------------------------------------------------
all() ->
    [roundtrip1].

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

roundtrip1() ->
    [{userdata,[{doc,"create a code for a URL and retrieve it"}]}].

roundtrip1(_Config) ->
    %% assumes a clean database

    Url1 = "url1",
    Ans1 = sherl_db:get_code(Url1),
    true = is_record(Ans1, url),
    Url1 = Ans1#url.url,
    1 = Ans1#url.code,

    Url2 = "url2",
    Ans2 = sherl_db:get_code(Url2),
    true = is_record(Ans2, url),
    Url2 = Ans2#url.url,
    2 = Ans2#url.code,

    ok.



