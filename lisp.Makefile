CASK ?= cask
export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

TESTSSRC = $(shell ls tests/*.el)
ELCTESTS = $(TESTSSRC:.el=.elc)
.DEFAULT_GOAL := compile

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

.PHONY: test
test: compile
	$(CASK) emacs -Q --batch -L ./lisp -L tests --eval "(setq package-user-dir \"$(CASK_DIR)\")" --eval "(package-initialize)" -l test-elpaso -f ert-run-tests-batch-and-exit

README.rst: README.in.rst lisp/elpaso.el
	grep ';;' lisp/elpaso.el \
	    | awk '/;;;\s*Commentary/{within=1;next}/;;;\s*/{within=0}within' \
	    | sed -e 's/^\s*;;*\s*//g' \
	    | tools/readme-sed.sh "COMMENTARY" README.in.rst > README.rst

.PHONY: compile
compile: cask
	! ($(CASK) eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)
	! ($(CASK) eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; $(CASK) clean-elc && exit $$ret)
	rm -f elpaso-autoloads.el

.PHONY: lint
lint: compile
	bash -ex tools/melpazoid.sh

.PHONY: clean
clean:
	$(CASK) clean-elc
	rm -f *autoloads.el
	rm -rf tests/test-install
	rm -rf melpazoid-master/elpaso

.PHONY: install
install: compile

define SET_GITHUB_ACTOR =
GITHUB_ACTOR := $(shell if [ -z ${GITHUB_ACTOR} ]; then git config user.name; else echo ${GITHUB_ACTOR} ; fi)
endef

define SET_GITHUB_ACTOR_REPOSITORY =
GITHUB_ACTOR_REPOSITORY := $(GITHUB_ACTOR)/$(shell basename `git rev-parse --show-toplevel`)
endef

define SET_GITHUB_HEAD_REF =
GITHUB_HEAD_REF := $(shell if [ -z ${GITHUB_HEAD_REF} ]; then git rev-parse --abbrev-ref HEAD; else echo ${GITHUB_HEAD_REF} ; fi)
endef

define SET_GITHUB_SHA =
GITHUB_SHA := $(shell if [ -z ${GITHUB_SHA} ] ; then git rev-parse origin/${GITHUB_HEAD_REF}; else echo ${GITHUB_SHA}; fi)
endef

define SET_GITHUB_COMMIT =
GITHUB_COMMIT := $(shell if git show -s --format=%s "${GITHUB_SHA}" | egrep -q "^Merge .* into" ; then git show -s --format=%s "${GITHUB_SHA}" | cut -d " " -f2 ; else echo "${GITHUB_SHA}" ; fi)
endef

.PHONY: test-install-vars
test-install-vars:
	$(eval $(call SET_GITHUB_ACTOR))
	$(eval $(call SET_GITHUB_ACTOR_REPOSITORY))
	$(eval $(call SET_GITHUB_HEAD_REF))
	$(eval $(call SET_GITHUB_SHA))
	$(eval $(call SET_GITHUB_COMMIT))
	git show -s --format=%s $(GITHUB_COMMIT)
	git show -s --format=%s $(GITHUB_SHA)
