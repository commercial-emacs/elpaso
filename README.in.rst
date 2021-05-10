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

.. COMMENTARY (see Makefile)

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
