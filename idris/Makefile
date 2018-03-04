IDRFLAGS = -p contrib --warnpartial --warnreach
SRCS = $(wildcard *.idr)

# There should be a better way.
.PHONY: $(SRCS)
*.idr:
	idris $(IDRFLAGS) $@ -o $(basename $@)
