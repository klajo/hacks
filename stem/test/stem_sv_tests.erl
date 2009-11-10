%%%-------------------------------------------------------------------
%%% @doc Test {@link stem_sv}.
%%% @author Klas Johansson (erlang@klasjohansson.se)
%%% @end
%%%-------------------------------------------------------------------
-module(stem_sv_tests).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-export([measure_kwords_per_second/0]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------
-define(orig_words_file,    "sv-orig.txt").
-define(stemmed_words_file, "sv-stemmed.txt").

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(stem_error, {orig_word, expected_stem, got_stem}).

stems_all_words_in_vocabulary_test() ->
    OrigWords = open_word_list(?orig_words_file),
    StemmedWords = open_word_list(?stemmed_words_file),
    [] = stem_words(OrigWords, StemmedWords).

measure_kwords_per_second() ->
    OrigWords = open_word_list(?orig_words_file),
    StemmedWords = open_word_list(?stemmed_words_file),
    NumWords = length(OrigWords),
    T1 = now(),
    Failures = stem_words(OrigWords, StemmedWords),
    NumFailed = length(Failures),
    T2 = now(),
    TDiff = timer:now_diff(T2, T1),
    io:format("~p of ~p stems (~.3f%) failed~n"
	      "~.1f kwords/s~n"
	      "~.1f us/word~n", 
	      [NumFailed, NumWords, NumFailed/NumWords*100, 
	       NumWords / (TDiff / 1000000) / 1000,
	       TDiff / NumWords]).

stem_words([OrigWord|OWRest], [StemmedWord|SWRest]) ->
    try stem_sv:stem(OrigWord) of
	StemmedWord -> 
	    stem_words(OWRest, SWRest);
	OtherWord ->
	    StemError = #stem_error{orig_word=OrigWord,
				    expected_stem=StemmedWord,
				    got_stem=OtherWord},
	    [StemError | stem_words(OWRest, SWRest)]
    catch
	Class:Reason ->
	    STrace = erlang:get_stacktrace(),
	    erlang:raise(Class, {failed_to_stem_word, OrigWord, Reason}, STrace)
    end;
stem_words([], []) ->
    [].

open_word_list(File) ->
    Dirname = filename:dirname(code:which(?MODULE)),
    {ok, Bin} = file:read_file(filename:join(Dirname, File)),
    string:tokens(binary_to_list(Bin), "\n").
