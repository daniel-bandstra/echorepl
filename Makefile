SUBDIRS = pedal engine

all:	$(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY:	$(SUBDIRS)
