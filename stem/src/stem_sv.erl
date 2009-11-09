%%%-------------------------------------------------------------------
%%% @doc 
%%% This is an implementation of the Swedish Porter stemming
%%% algorithm as described here:
%%%
%%%     http://snowball.tartarus.org/algorithms/swedish/stemmer.html
%%%
%%% The implementation has been tested on the sample Swedish
%%% vocabulary and produces an identical result when compared to its
%%% stemmed equivalent.
%%%
%%% The steps below utilize regular erlang pattern matching, but stem
%%% the words backwards since the Porter algorithm works by checking
%%% the end of the words, but patterns match from the front.
%%% @author Klas Johansson (klas.johansson@gmail.com)
%%% @end
%%%-------------------------------------------------------------------
-module(stem_sv).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([stem/1]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([]).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------
-define(is_vowel(C),
	((C == $a)
	 or (C == $e)
	 or (C == $i)
	 or (C == $o)
	 or (C == $u)
	 or (C == $y)
	 or (C == $å)
	 or (C == $ä)
	 or (C == $ö))).

-define(is_valid_s_ending(C),
	((C == $b)
	 or (C == $c)
	 or (C == $d)
	 or (C == $f)
	 or (C == $g)
	 or (C == $h)
	 or (C == $j)
	 or (C == $k)
	 or (C == $l)
	 or (C == $m)
	 or (C == $n)
	 or (C == $o)
	 or (C == $p)
	 or (C == $r)
	 or (C == $t)
	 or (C == $v)
	 or (C == $y))).

-define(covers_r1(W, R), length(W) >= R#r.r1).
	       
-define(if_covers_r1(W, R, Yes, No),
	if ?covers_r1(W, R) -> Yes;
	   true             -> No
	end).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(r, {r1}).


%%--------------------------------------------------------------------
%% @spec stem(Word) -> Stem
%%         Word = string()
%%         Stem = string()
%% @doc Return the stem of a word.
%% @end
%%--------------------------------------------------------------------
stem(W0) ->
    R1pos = find_r_pos(W0),
    R = #r{r1=R1pos},
    W1 = lists:reverse(W0),
    W2 = run_steps(W1, R),
    lists:reverse(W2).

run_steps(W, R) ->
    W1  = step_1(W, R),
    W2 = step_2(W1, R),
    step_3(W2, R).

%% step 1
step_1("anreteh"++W, R) when ?covers_r1(W, R) -> W;
step_1("sneteh"++W,  R) when ?covers_r1(W, R) -> W;
step_1("tedna"++W,   R) when ?covers_r1(W, R) -> W;
step_1("snera"++W,   R) when ?covers_r1(W, R) -> W;
step_1("sedna"++W,   R) when ?covers_r1(W, R) -> W;
step_1("sanro"++W,   R) when ?covers_r1(W, R) -> W;
step_1("sanre"++W,   R) when ?covers_r1(W, R) -> W;
step_1("sanra"++W,   R) when ?covers_r1(W, R) -> W;
step_1("reteh"++W,   R) when ?covers_r1(W, R) -> W;
step_1("neteh"++W,   R) when ?covers_r1(W, R) -> W;
step_1("nedna"++W,   R) when ?covers_r1(W, R) -> W;
step_1("snre"++W,    R) when ?covers_r1(W, R) -> W;
step_1("seda"++W,    R) when ?covers_r1(W, R) -> W;
step_1("nera"++W,    R) when ?covers_r1(W, R) -> W;
step_1("etsa"++W,    R) when ?covers_r1(W, R) -> W;
step_1("enra"++W,    R) when ?covers_r1(W, R) -> W;
step_1("edna"++W,    R) when ?covers_r1(W, R) -> W;
step_1("anro"++W,    R) when ?covers_r1(W, R) -> W;
step_1("anre"++W,    R) when ?covers_r1(W, R) -> W;
step_1("anra"++W,    R) when ?covers_r1(W, R) -> W;
step_1("tsa"++W,     R) when ?covers_r1(W, R) -> W;
step_1("teh"++W,     R) when ?covers_r1(W, R) -> W;
step_1("sne"++W,     R) when ?covers_r1(W, R) -> W;
step_1("nre"++W,     R) when ?covers_r1(W, R) -> W;
step_1("era"++W,     R) when ?covers_r1(W, R) -> W;
step_1("eda"++W,     R) when ?covers_r1(W, R) -> W;
step_1("ta"++W,      R) when ?covers_r1(W, R) -> W;
step_1("se"++W,      R) when ?covers_r1(W, R) -> W;
step_1("sa"++W,      R) when ?covers_r1(W, R) -> W;
step_1("ro"++W,      R) when ?covers_r1(W, R) -> W;
step_1("re"++W,      R) when ?covers_r1(W, R) -> W;
step_1("ra"++W,      R) when ?covers_r1(W, R) -> W;
step_1("ne"++W,      R) when ?covers_r1(W, R) -> W;
step_1("da"++W,      R) when ?covers_r1(W, R) -> W;
step_1("e"++W,       R) when ?covers_r1(W, R) -> W;
step_1("a"++W,       R) when ?covers_r1(W, R) -> W;
step_1("s"++W,       R) when ?covers_r1(W, R), ?is_valid_s_ending(hd(W)) -> W;
step_1(W, _R)                                 -> W.

%% step 2
step_2("dd"++W, R) when ?covers_r1(W, R) -> "d"++W;
step_2("dg"++W, R) when ?covers_r1(W, R) -> "g"++W;
step_2("nn"++W, R) when ?covers_r1(W, R) -> "n"++W;
step_2("td"++W, R) when ?covers_r1(W, R) -> "d"++W;
step_2("tg"++W, R) when ?covers_r1(W, R) -> "g"++W;
step_2("tk"++W, R) when ?covers_r1(W, R) -> "k"++W;
step_2("tt"++W, R) when ?covers_r1(W, R) -> "t"++W;
step_2(W, _R)                            -> W.


%% step 3
step_3("gil"++W,   R) when ?covers_r1(W, R) -> W;
step_3("gi"++W,    R) when ?covers_r1(W, R) -> W;
step_3("sle"++W,   R) when ?covers_r1(W, R) -> W;
step_3("tsöl"++W,  R) when ?covers_r1(W, R) -> "söl"++W;
step_3("tlluf"++W, R) when ?covers_r1(W, R) -> "lluf"++W;
step_3(W, _R)                               -> W.


%%----------------------------------------------------------------------
find_r_pos(W0) ->
    {_W, R1pos} = find_r_pos_2(W0, 0),
    if R1pos =< 3 -> 3;
       true       -> R1pos
    end.

find_r_pos_2([C1,C2|W], N) when ?is_vowel(C1), not ?is_vowel(C2) ->
    {W, N+2};
find_r_pos_2([_C|W], N) ->
    find_r_pos_2(W, N+1);
find_r_pos_2([], N) ->
    {[], N}.
