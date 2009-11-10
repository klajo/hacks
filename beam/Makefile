SRC_FILES=$(wildcard src/*.erl)
BEAM_FILES=$(SRC_FILES:src/%.erl=ebin/%.beam)

TEST_SRC_FILES=$(wildcard test/*.erl)
TEST_BEAM_FILES=$(TEST_SRC_FILES:test/%.erl=test/%.beam)

all: ebin $(BEAM_FILES)

test: all $(TEST_BEAM_FILES)
	@erl -pa ebin \
	     -pa test \
	     -noinput \
	     -boot start_clean \
	     -eval 'eunit:test("test", [verbose]).' \
	     -s init stop

ebin:
	mkdir ebin

ebin/%.beam: src/%.erl
	erlc -o ebin $<

test/%.beam: test/%.erl
	erlc -o test $<

clean:
	@rm -f $(BEAM_FILES) $(TEST_BEAM_FILES)




