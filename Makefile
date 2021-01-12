.PHONY: all bin/hepl bin/lars

all: bin/hepl bin/lars

ivory.pill:
	curl -Lo ivory.pill https://github.com/urbit/urbit/raw/master/bin/ivory.pill

bin/hepl: ivory.pill
	@sbcl --noinform \
		--eval '(asdf:make "cl-urbit/hepl")' \
		--eval '(sb-ext:exit :code 0)'

bin/lars: ivory.pill
	@sbcl --noinform \
		--eval '(asdf:make "cl-urbit/lars")' \
		--eval '(sb-ext:exit :code 0)'

#--control-stack-size 90 \
#--dynamic-space-size 2048 \
