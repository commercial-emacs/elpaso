|build-status|

+-------------------------+-------------------------+
|Bob Slydell:             |*So you physically take  |
|                         |the specs from the       |
|                         |customer?*               |
+-------------------------+-------------------------+
|Tom Smykowski:           |Well... no. My secretary |
|                         |does that, or the fax.   |
+-------------------------+-------------------------+
|Bob Porter:              |*So then you must        |
|                         |physically bring them to |
|                         |the software people?*    |
+-------------------------+-------------------------+
|Tom Smykowski:           |Well. No... yeah, I mean |
|                         |sometimes!               |
+-------------------------+-------------------------+

|---| *Office Space* (1999)

=================================================
 elpaso -- Emacs Lisp Package Archive Self Officiator
=================================================

Self-officiating package manager for the emacs text editor.  Like quelpa_.

Elpaso abjures the middle-man role played by package archive operators like
ELPA and MELPA, building packages directly from upstream sources.

``M-x elpaso``
  Specifying owner/package, e.g., ``magnars/dash.el`` yields the desired github **or
  gitlab** repo.  More conveniently, enter free-form keywords to conduct
  a github search.  As of this writing, gitlab's nascent search
  functionality is unviable.  *First-time users will need to authenticate with
  both github and gitlab's GraphQL API.*

  If the target has not registered a recipe, elpaso proceeds with package.el's
  baseline assumptions.  Alas most packages in the wild do not adhere to package.el's
  somewhat arbitrary demands.  See FAQ below to assist elpaso's efforts.

``M-x elpaso-refresh``
  Refresh recipes from all sources in ``elpaso-admin-cookbooks`` (defaults to
  melpa, elpa, and nongnu).

``M-x elpaso-install``
  Enter the package name to install or reinstall.

``M-x elpaso-delete``
  Enter the package name, then ``TAB``, to delete.

``M-x elpaso-purge``
  Deletes residual files in ``elpaso-defs-toplevel-dir`` (defaults to ``~/.emacs.d/elpaso``).

Install
=======
Clone and ``make install``.

Frequently Asked Questions
==========================

... How can I advance elpaso's cause?
    Elpaso seeks to realize an autonomous, gatekeeper-less future.  For simple packages, package authors need only ensure ``M-: (package-buffer-info)`` parses when visiting their ``<name>.el`` main file.  For multi-file packages, ``git add .recipe`` to your toplevel directory a suitable recipe in the MELPA_ or ELPA_ format, e.g.,
    ::

        (my-package :url "github.com/my-package.git"
                    :files ("my-package*.el" "and-subdirectory"))

... But I like quelpa.
    Quelpa is essentially MELPA's vassal, having slavishly copy-pasted the `package-build`_ code, and thus hardwires MELPA's recipe format, and inherits MELPA's erroneous versioning.

... Where did you put my pre-existing installation of package XYZ?
    I put it in ``~/.emacs.d/elpaso/backups``.

... How can I integrate this with ``use-package``?
    ::

        M-x customize-option RET use-package-ensure-function RET
        Custom: elpaso-use-package-ensure-function

... How do I pin commits, freeze working configurations, do everything that Straight does?
    You don't, I'm afraid.

... How can I edit packages in-place like Straight?
    Come down from the ledge.  Fork-clone the package as you normally would, say in ``/home/kilroy/package``.
    Then add to the list in ``~/.emacs.d/elpaso/recipes/user/recipes``, the entry::

        ;; -*- lisp-data -*-
        ((package :url "/home/kilroy/package" :files ("*.el" "lisp/*.el")))

    (the recipe will vary), followed by ``M-x elpaso-refresh``, and ``M-x elpaso-install``.

... Why did elpaso unnecessarily fetch a dependency?
    Elpaso can't know whether a package-require such as ``(dash 20210401)`` signifies a bonafide  ``v20210401`` that the dash author intended or a MELPA-imposed hack.

Some `uninteresting comments`_ about the ELPAs.

.. _Getting started: http://melpa.org/#/getting-started
.. _Issue 2944: https://github.com/melpa/melpa/issues/2944
.. _Advising Functions: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
.. _reimplementing their service: https://github.com/dickmao/shmelpa
.. _quelpa: https://github.com/quelpa/quelpa
.. _package-build: https://github.com/melpa/package-build
.. _uninteresting comments: https://raw.githubusercontent.com/dickmao/elpaso/dev/elpas.txt
.. _MELPA: https://github.com/melpa/melpa#recipe-format
.. _ELPA: https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/README

.. |build-status|
   image:: https://github.com/dickmao/elpaso/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/elpaso/actions
   :alt: Build Status

.. |---| unicode:: U+02014 .. em dash
   :trim:
