FILES = $(wildcard *.pl)
RULES = $(patsubst %.pl,%,$(FILES))

all: $(RULES) run
	@echo $(FILES)

%: %.pl
	swipl -quiet -O -g main --stand_alone=true -o $@ -c $@.pl

run:
	./$(RULES)

clean: $(RULES)
	rm $(RULES) clauses header infile.cnf model