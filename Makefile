LIBS = sherl sherlweb

all : $(LIBS)

$(LIBS) :
	@cd lib/$@; $(MAKE) all

clean :
	@for dir in $(LIBS); do \
          (cd lib/$$dir; $(MAKE) $@) \
        done

.PHONY: $(LIBS)