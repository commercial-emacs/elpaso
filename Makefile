export EMACS ?= emacs
CASK=$(shell which cask)

DEBUG=

ifdef ELPASO_DEBUG
DEBUG:=${DEBUG} --eval "(setq elpaso-admin--debug t)" --eval "(setq debug-on-error t)"
endif

ifdef DONT_PHONE_HOME
DEBUG:=${DEBUG} --eval "(custom-set-default (quote elpaso-admin-cookbooks) (quote (user)))"
endif

EMACSBATCH=$(EMACS) -Q --batch -L ./lisp -l cl-lib -l elpaso-dev -f elpaso-dev $(DEBUG)
RM=rm -f
PKG_DESCS_MK=.pkg-descs.mk

ifneq ($(CASK),)
CASK_DIR := $(shell $(CASK) package-directory)

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	cask install
	touch $(CASK_DIR)
endif


TESTSSRC := $(shell ls test/*.el)
ELCTESTS := $(TESTSSRC:.el=.elc)

ifneq ($(CASK),)
.PHONY: compile
compile: cask
	! (cask eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; cask clean-elc && exit $$ret)
	! (cask eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)
	rm -f elpaso-autoloads.el

.PHONY: lint
lint: compile
	bash -eux tools/melpazoid.sh

.PHONY: test
test: compile
	cask exec ert-runner --reporter ert $(TESTSSRC)
endif

pkgs := $(wildcard packages/*)
autoloads := $(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-autoloads.el)
descs := $(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-pkg.el)

.PHONY: refresh
refresh: $(autoloads) $(descs) $(pkgs)

.PHONY: clean clean/%
clean:
	$(RM) $(PKG_DESCS_MK)
	for pkg in $(notdir $(pkgs)) ; do make clean/$${pkg} ; done

clean/%:
	$(RM) $(filter packages/$*/%, $(autoloads)) $(filter packages/$*/%, $(descs))
	find $(filter packages/$*, $(pkgs)) -name '*.elc' -print0 | xargs -0 $(RM)

.PHONY: refresh/%
refresh/%:
	@$(EMACSBATCH) -f elpaso-admin-batch-refresh "$*"

.PHONY: tidy/%
tidy/%:
	@$(EMACSBATCH) -f elpaso-admin-batch-tidy "$*"

.PHONY: fetch/%
fetch/%:
	@$(EMACSBATCH) -f elpaso-admin-batch-fetch "$*"

.PHONY: build/%
build/%:
	@$(EMACSBATCH) -f elpaso-admin-batch-build "$*"

.PHONY: install/%
install/%:
	@$(EMACSBATCH) -f elpaso-admin-batch-install "$*"

.PHONY: install
install:
	git clone . bootstrap
	cd bootstrap ; ! git clean -ndfX | grep -q .
	cd bootstrap ; DONT_PHONE_HOME=t $(MAKE) install/elpaso
	if [ -z $${ELPASO_DEBUG} ] ; then rm -rf bootstrap ; fi

.PHONY: debug-install
debug-install:
	ELPASO_DEBUG=t $(MAKE) install

.PHONY: hulk-smash
hulk-smash:
	@$(EMACSBATCH) -f elpaso-admin-purge

README.rst: README.in.rst lisp/elpaso.el Makefile
	grep ';;' lisp/elpaso.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;\s\?//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst
