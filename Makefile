export EMACS ?= emacs
ifdef ELPASO_DEBUG
DEBUG=--eval "(setq elpaso-admin--debug t)" --eval "(setq debug-on-error t)"
else
DEBUG=
endif
EMACSBATCH=$(EMACS) -Q --batch -L ./lisp -l elpaso-admin $(DEBUG)
RM=rm -f
PKG_DESCS_MK=.pkg-descs.mk

CASK_DIR := $(shell cask package-directory)

.PHONY: cask
cask: $(CASK_DIR)

$(CASK_DIR): Cask
	cask install
	touch $(CASK_DIR)

TESTSSRC := $(shell ls test/*.el)
ELCTESTS := $(TESTSSRC:.el=.elc)

.PHONY: compile
compile: cask
	! (cask eval "(let ((byte-compile-error-on-warn t)) (cask-cli/build))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; cask clean-elc && exit $$ret)
	! (cask eval \
	      "(cl-letf (((symbol-function (quote cask-files)) (lambda (&rest _args) (mapcar (function symbol-name) (quote ($(TESTSSRC))))))) \
	          (let ((byte-compile-error-on-warn t)) (cask-cli/build)))" 2>&1 | egrep -a "(Warning|Error):") ; (ret=$$? ; rm -f $(ELCTESTS) && exit $$ret)
	rm -f elpaso-autoloads.el

.PHONY: lint
lint: compile
	bash -eu tools/melpazoid.sh

pkgs := $(wildcard packages/*)
autoloads := $(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-autoloads.el)
descs := $(foreach pkg, $(pkgs), $(pkg)/$(notdir $(pkg))-pkg.el)

.PHONY: refresh
refresh: $(autoloads) $(descs) $(pkgs)

.PHONY: check/% check-all
check-all: check/-
check/%:
	$(EMACSBATCH) -f elpaso-admin-batch-copyright-check $*

# this calls elpaso-admin--worktree-sync
# so subsumes make packages/%
.PHONY: clean clean/%
clean:
	$(RM) $(PKG_DESCS_MK)
	for pkg in $(notdir $(pkgs)) ; do make clean/$${pkg} ; done

clean/%:
	$(RM) $(filter packages/$*/%, $(autoloads)) $(filter packages/$*/%, $(descs))
	find $(filter packages/$*, $(pkgs)) -name '*.elc' -print0 | xargs -0 $(RM)

.PHONY: readme
readme:
	$(EMACSBATCH) -l org --eval "(find-file \"README\")" \
	  --eval "(org-export-to-file (quote html) \"html/readme.html\")"

.PHONY: refresh/%
refresh/%:
	@$(EMACSBATCH) -f elpaso-admin-batch-refresh "$*"

.PHONY: remove/%
remove/%:
	@$(EMACSBATCH) -f elpaso-admin-batch-remove "$*"

.PHONY: fetch/%
fetch/%:
	@$(EMACSBATCH) -f elpaso-admin-batch-fetch "$*"

.PHONY: build/%
build/%: fetch/%
	@$(EMACSBATCH) -f elpaso-admin-batch-build "$*"

.PHONY: install/%
install/%: build/%
	@$(EMACSBATCH) -f elpaso-admin-batch-install "$*"

.PHONY: test
test: compile
	for file in test/mockhub.com/{package,recipes}.git ; do cd $$file ; rm -rf .git ; git init ; git add . ; git commit -am "initial commit" ; done
	cask exec ert-runner --reporter ert $(TESTSSRC)

.PHONY: hulk-smash
hulk-smash:
	@$(EMACSBATCH) -f elpaso-admin-purge
