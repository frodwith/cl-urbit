.PHONY: all bin/hepl

all: bin/hepl

ivory.pill:
	curl -Lo ivory.pill https://github.com/urbit/urbit/raw/master/bin/ivory.pill

bin/hepl: ivory.pill
	@sbcl --noinform \
		--eval '(asdf:make "cl-urbit/hepl")' \
		--eval '(sb-ext:exit :code 0)'
