SUBDIRS = pedal sample tape

all:	$(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

.PHONY:	$(SUBDIRS)
