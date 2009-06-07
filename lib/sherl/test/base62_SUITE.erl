-module(base62_SUITE).

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
    [digit_to_char, char_to_digit].

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

digit_to_char() ->
    [{userdata,[{doc,"digit to 0-9A-Za-z"}]}].

digit_to_char(_Config) ->
    [ Char = base62:digit_to_char(Digit) ||
        {Digit, Char} <- lists:zip(all_digits(), all_chars()) ],
    ok.

char_to_digit(_Config) ->
    [ Digit = base62:char_to_digit(Char) ||
        {Digit, Char} <- lists:zip(all_digits(), all_chars()) ],
    ok.

%% internal helpers

all_chars() ->
    [$0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $A, $B, $C, $D, $E, $F,
     $G, $H, $I, $J, $K, $L, $M, $N, $O, $P, $Q, $R, $S, $T, $U, $V,
     $W, $X, $Y, $Z, $a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l,
     $m, $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z].

all_digits() ->
    lists:seq(0, 61).

