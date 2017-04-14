FILES = $(wildcard *.pl)
RULES = $(patsubst %.pl,%,$(FILES))

all: $(RULES) run
	@echo $(FILES)

%: %.pl
	swipl -O -q --stand_alone=true -o $@ -c $@.pl

run:
	./$(RULES)

clean: $(RULES)
	rm $(RULES)