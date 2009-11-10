%%%-------------------------------------------------------------------
%%% @doc 
%%% This is an implementation of the English Porter2 stemming
%%% algorithm as described here:
%%%
%%%     http://snowball.tartarus.org/algorithms/english/stemmer.html
%%%
%%% The implementation has been tested on the sample English
%%% vocabulary and produces an identical result when compared to its
%%% stemmed reference equivalent.
%%%
%%% The steps below utilize regular erlang pattern matching, but stem
%%% the words backwards since the Porter2 algorithm works by checking
%%% the end of the words, but patterns match from the front.
%%% @author Klas Johansson (erlang@klasjohansson.se)
%%% @end
%%%-------------------------------------------------------------------
-module(stem_en).

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
	 or (C == $y))).

-define(is_double(C1,C2),
	(C1 == C2)
	and
	((C1 == $b)
	 or (C1 == $d)
	 or (C1 == $f)
	 or (C1 == $g)
	 or (C1 == $m)
	 or (C1 == $n)
	 or (C1 == $p)
	 or (C1 == $r)
	 or (C1 == $t))).

-define(is_consonant(C), not ?is_vowel(C)).

-define(is_valid_li_ending(C),
	((C == $c)
	 or (C == $d)
	 or (C == $e)
	 or (C == $g)
	 or (C == $h)
	 or (C == $k)
	 or (C == $m)
	 or (C == $n)
	 or (C == $r)
	 or (C == $t))).

-define(covers_r1(W, R), length(W) >= R#r.r1).
-define(covers_r2(W, R), length(W) >= R#r.r2).
	       
-define(if_covers_r1(W, R, Yes, No),
	if ?covers_r1(W, R) -> Yes;
	   true             -> No
	end).
-define(if_covers_r2(W, R, Yes, No),
	if ?covers_r2(W, R) -> Yes;
	   true             -> No
	end).

-define(r1_is_null(W, R), length(W) == R#r.r1).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(r, {r1, r2}).


%%--------------------------------------------------------------------
%% @spec stem(Word) -> Stem
%%         Word = string()
%%         Stem = string()
%% @doc Return the stem of a word.
%% @end
%%--------------------------------------------------------------------

%% special stems
stem("skis")   -> "ski";
stem("skies")  -> "sky";
stem("dying")  -> "die";
stem("lying")  -> "lie";
stem("tying")  -> "tie";
stem("idly")   -> "idl";
stem("gently") -> "gentl";
stem("ugly")   -> "ugli";
stem("early")  -> "earli";
stem("only")   -> "onli";
stem("singly") -> "singl";
%% invariants
stem("sky")    -> "sky"; 
stem("news")   -> "news"; 
stem("howe")   -> "howe"; 
stem("atlas")  -> "atlas"; 
stem("cosmos") -> "cosmos"; 
stem("bias")   -> "bias"; 
stem("andes")  -> "andes";
%% 
stem(W0) when length(W0) > 2 ->
    W1 = step_pre0(W0),
    {R1pos, R2pos} = find_r_pos(W1),
    R = #r{r1=R1pos, r2=R2pos},
    W2 = lists:reverse(W1),
    W3 = run_steps(W2, R),
    step_post(W3);
stem(W) ->
    W.

run_steps(W, R) ->
    W0  = step_0(W),
    W1a = step_1a(W0),
    if W1a == "gninni";
       W1a == "gnituo";
       W1a == "gninnac";
       W1a == "gnirreh";
       W1a == "gnirrae";
       W1a == "deecorp";
       W1a == "deecxe";
       W1a == "deeccus" ->
	    W1a;
       true -> 
	    W1b = step_1b(W1a, R),
	    W1c = step_1c(W1b, R),
	    W2  = step_2(W1c, R),
	    W3  = step_3(W2, R), 
	    W4  = step_4(W3, R),
	    step_5(W4, R)
    end.

%% pre steps
step_pre0(W) ->
    step_pre0_b(step_pre0_a(W)).

step_pre0_a([$\'|W]) -> W;
step_pre0_a(W)       -> W.

step_pre0_b([$y|W]) -> [$Y | step_pre0_b_2(W)];
step_pre0_b(W)      -> step_pre0_b_2(W).

step_pre0_b_2([V,$y|W]) when ?is_vowel(V) -> [V,$Y|step_pre0_b_2(W)];
step_pre0_b_2([C|W])                      -> [C|step_pre0_b_2(W)];
step_pre0_b_2([])                         -> [].

%% step 0
step_0("'s'"++W) -> W;
step_0("s'"++W)  -> W;
step_0("'"++W)   -> W;
step_0(W)        -> W.

%% step 1a
step_1a("sess"++W)     -> "ss"++W;
step_1a("dei"++W)      ->
    if length(W) > 1   -> "i"++W;
       true            -> "ei"++W
    end;
step_1a("sei"++W)      ->
    if length(W) > 1   -> "i"++W;
       true            -> "ei"++W
    end;
step_1a("ss"++_ = W)   -> W;
step_1a("su"++_ = W)   -> W;
step_1a([$s,C|W] = W0) ->
    case has_vowel(W) of
	true  -> [C|W];
	false -> W0
    end;
step_1a(W)             -> W.

%% step 1b
step_1b("yldee"++W = W0, R) -> ?if_covers_r1(W, R, "ee"++W, W0);
step_1b("dee"++W = W0, R)   -> ?if_covers_r1(W, R, "ee"++W, W0);
step_1b("de"++W = W0, R)    -> step_1b_3(W0, W, R);
step_1b("ylde"++W = W0, R)  -> step_1b_3(W0, W, R);
step_1b("gni"++W = W0, R)   -> step_1b_3(W0, W, R);
step_1b("ylgni"++W = W0, R) -> step_1b_3(W0, W, R);
step_1b(W, _R)              -> W.

step_1b_3(W0, W, R) ->   
    case has_vowel(W) of
	true  -> step_1b_4(W, R);
	false -> W0
    end.

step_1b_4("ta"++_ = W, _R)                   -> "e"++W;
step_1b_4("lb"++_ = W, _R)                   -> "e"++W;
step_1b_4("zi"++_ = W, _R)                   -> "e"++W;
step_1b_4([C,C|W], _R) when ?is_double(C, C) -> [C|W];
step_1b_4(W, R)                              ->
    case is_short_word(W, R) of
	true  -> "e"++W;
	false -> W
    end.

%% step 1c
step_1c([$y,C|W], _R) when not ?is_vowel(C), length(W) > 0 -> [$i,C|W];
step_1c([$Y,C|W], _R) when not ?is_vowel(C), length(W) > 0 -> [$i,C|W];
step_1c(W, _R)                                             -> W.    

%% step 2
step_2("lanoita"++W = W0, R) -> ?if_covers_r1(W, R, "eta"++W, W0);
step_2("lanoit"++W = W0, R)  -> ?if_covers_r1(W, R, "noit"++W, W0);
step_2("icne"++W = W0, R)    -> ?if_covers_r1(W, R, "ecne"++W, W0);
step_2("icna"++W = W0, R)    -> ?if_covers_r1(W, R, "ecna"++W, W0);
step_2("ilba"++W = W0, R)    -> ?if_covers_r1(W, R, "elba"++W, W0);
step_2("iltne"++W = W0, R)   -> ?if_covers_r1(W, R, "tne"++W, W0);
step_2("rezi"++W = W0, R)    -> ?if_covers_r1(W, R, "ezi"++W, W0);
step_2("noitazi"++W = W0, R) -> ?if_covers_r1(W, R, "ezi"++W, W0);
step_2("noita"++W = W0, R)   -> ?if_covers_r1(W, R, "eta"++W, W0);
step_2("rota"++W = W0, R)    -> ?if_covers_r1(W, R, "eta"++W, W0);
step_2("msila"++W = W0, R)   -> ?if_covers_r1(W, R, "la"++W, W0);
step_2("itila"++W = W0, R)   -> ?if_covers_r1(W, R, "la"++W, W0);
step_2("illa"++W = W0, R)    -> ?if_covers_r1(W, R, "la"++W, W0);
step_2("ssenluf"++W = W0, R) -> ?if_covers_r1(W, R, "luf"++W, W0);
step_2("ilsuo"++W = W0, R)   -> ?if_covers_r1(W, R, "suo"++W, W0);
step_2("ssensuo"++W = W0, R) -> ?if_covers_r1(W, R, "suo"++W, W0);
step_2("ssenevi"++W = W0, R) -> ?if_covers_r1(W, R, "evi"++W, W0);
step_2("itivi"++W = W0, R)   -> ?if_covers_r1(W, R, "evi"++W, W0);
step_2("itilib"++W = W0, R)  -> ?if_covers_r1(W, R, "elb"++W, W0);
step_2("ilb"++W = W0, R)     -> ?if_covers_r1(W, R, "elb"++W, W0);
step_2("igo"++W = W0, R)     -> ?if_covers_r1(W, R, 
					      if hd(W) == $l -> "go"++W;
						 true        -> W0
					      end,
					      W0);
step_2("illuf"++W = W0, R)   -> ?if_covers_r1(W, R, "luf"++W, W0);
step_2("ilssel"++W = W0, R)  -> ?if_covers_r1(W, R, "ssel"++W, W0);
step_2([$i,$l|W] = W0, R)    -> ?if_covers_r1(W, R,
					      if ?is_valid_li_ending(hd(W))->W;
						 true                      ->W0
					      end,
					      W0);
step_2(W, _R)                -> W.

%% step 3
step_3("lanoita"++W = W0, R) -> ?if_covers_r1(W, R, "eta"++W, W0);
step_3("lanoit"++W = W0, R)  -> ?if_covers_r1(W, R, "noit"++W, W0);
step_3("ezila"++W = W0, R)   -> ?if_covers_r1(W, R, "la"++W, W0);
step_3("etaci"++W = W0, R)   -> ?if_covers_r1(W, R, "ci"++W, W0);
step_3("itici"++W = W0, R)   -> ?if_covers_r1(W, R, "ci"++W, W0);
step_3("laci"++W = W0, R)    -> ?if_covers_r1(W, R, "ci"++W, W0);
step_3("luf"++W = W0, R)     -> ?if_covers_r1(W, R, W, W0);
step_3("ssen"++W = W0, R)    -> ?if_covers_r1(W, R, W, W0);
step_3("evita"++W = W0, R)   -> ?if_covers_r2(W, R, W, W0);
step_3(W, _R)                -> W.

% step 4
step_4("la"++W = W0, R)    -> ?if_covers_r2(W, R, W, W0);
step_4("ecna"++W = W0, R)  -> ?if_covers_r2(W, R, W, W0);
step_4("ecne"++W = W0, R)  -> ?if_covers_r2(W, R, W, W0);
step_4("re"++W = W0, R)    -> ?if_covers_r2(W, R, W, W0);
step_4("ci"++W = W0, R)    -> ?if_covers_r2(W, R, W, W0);
step_4("elba"++W = W0, R)  -> ?if_covers_r2(W, R, W, W0);
step_4("elbi"++W = W0, R)  -> ?if_covers_r2(W, R, W, W0);
step_4("tna"++W = W0, R)   -> ?if_covers_r2(W, R, W, W0);
step_4("tneme"++W = W0, R) -> ?if_covers_r2(W, R, W, W0);
step_4("tnem"++W = W0, R)  -> ?if_covers_r2(W, R, W, W0);
step_4("tne"++W = W0, R)   -> ?if_covers_r2(W, R, W, W0);
step_4("msi"++W = W0, R)   -> ?if_covers_r2(W, R, W, W0);
step_4("eta"++W = W0, R)   -> ?if_covers_r2(W, R, W, W0);
step_4("iti"++W = W0, R)   -> ?if_covers_r2(W, R, W, W0);
step_4("suo"++W = W0, R)   -> ?if_covers_r2(W, R, W, W0);
step_4("evi"++W = W0, R)   -> ?if_covers_r2(W, R, W, W0);
step_4("ezi"++W = W0, R)   -> ?if_covers_r2(W, R, W, W0);
step_4("noi"++W = W0, R)   -> ?if_covers_r2(W, R, 
					    case hd(W) of
						$s -> W;
						$t -> W;
						_  -> W0
					    end,
					    W0);
step_4(W, _R)              -> W.
    
%% step 5 
step_5("e"++W, R) when ?covers_r2(W, R)              -> W;
step_5("e"++W = W0, R) when ?covers_r1(W, R)         -> 
    case ends_in_short_syllable(W) of
	true  -> W0;
	false -> W
    end;
step_5("l"++W, R) when hd(W) == $l, ?covers_r2(W, R) -> W;
step_5(W, _R)                                        -> W.
    
%% post steps
step_post(W) ->
    step_post_2(W, []).

step_post_2([$Y|W], Acc) -> step_post_2(W, [$y|Acc]);
step_post_2([C|W], Acc)  -> step_post_2(W, [C|Acc]);
step_post_2([], Acc)     -> Acc.

%%----------------------------------------------------------------------
has_vowel([C|W]) ->
    if ?is_vowel(C) -> true;
       true         -> has_vowel(W)
    end;
has_vowel([]) ->
    false.


find_r_pos("gener"++W1) ->
    R1pos = 5,  % the length of `gener'
    {_W, R2pos} = find_r_pos_2(W1, R1pos),
    {R1pos, R2pos};
find_r_pos("commun"++W1) ->
    R1pos = 6,  % the length of `commun'
    {_W, R2pos} = find_r_pos_2(W1, R1pos),
    {R1pos, R2pos};
find_r_pos(W0) ->
    {W1, R1pos} = find_r_pos_2(W0, 0),
    {_W, R2pos} = find_r_pos_2(W1, R1pos),
    {R1pos, R2pos}.

find_r_pos_2([C1,C2|W], N) when ?is_vowel(C1), not ?is_vowel(C2) ->
    {W, N+2};
find_r_pos_2([_C|W], N) ->
    find_r_pos_2(W, N+1);
find_r_pos_2([], N) ->
    {[], N}.


%% Define a short syllable in a word as either 
%% (a) a vowel followed by a non-vowel other than w, x or Y and preceded
%%     by a non-vowel, or
%% (b) a vowel at the beginning of the word followed by a non-vowel.
ends_in_short_syllable([C,V]) when ?is_vowel(V), not ?is_vowel(C) -> 
    true;
ends_in_short_syllable([C1,V,C2|_]) when not ?is_vowel(C1), 
                                         ((C1 =/= $w) 
					  and (C1 =/= $x) 
					  and (C1 =/= $Y)),
                                         ?is_vowel(V),
                                         not ?is_vowel(C2) ->
    true;
ends_in_short_syllable(_W) ->
    false.

%% A word is called short if it consists of a short syllable preceded
%% by zero or more consonants.
is_short_word(W, R) when ?r1_is_null(W, R) ->
    true;
is_short_word([C1,V,C2|W], _R) when not ?is_vowel(C1), 
				    ((C1 =/= $w)
				     and (C1 =/= $x)
				     and (C1 =/= $Y)),
				    ?is_vowel(V),
				    not ?is_vowel(C2) ->
    lists:all(fun(C) -> ?is_consonant(C) end, W);
is_short_word([C,V], _R) when not ?is_vowel(C), ?is_vowel(V) ->
    true;
is_short_word(_W, _R) ->
   false.
			
						 
