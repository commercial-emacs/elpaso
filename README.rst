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
  You may enter the full owner/package of a github or gitlab repository, e.g.,
  ``magnars/dash.el``, or free-form keywords.  If the latter, only github is searched.
  As of this writing, gitlab's nascent search functionality is too rough hewn.

  If you've not registered a recipe, elpaso will happily attempt to package
  your repo by consulting your ``<name>-pkg.el`` file, or in lieu of that, running
  ``package-buffer-info`` on your ``<name>.el``.  You can
  help elpaso's cause by opening your ``<name>.el``, and running
  ``M-: (package-buffer-info)`` to ensure it produces a proper package descriptor.

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

... But I like quelpa.
    Quelpa is less likely to fail spectacularly, yes, but its slavish copy-paste of MELPA's `package-build`_ code inherits the latter's erroneous versioning.  Quelpa also cannot handle GNU or NonGNU ELPA recipes (or in FSF nomenclature "specs") without changes.

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
        (
         (package :url "/home/kilroy/package" :files ("*.el" "lisp/*.el"))
        )

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

.. |build-status|
   image:: https://github.com/dickmao/elpaso/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/elpaso/actions
   :alt: Build Status

.. |---| unicode:: U+02014 .. em dash
   :trim:
