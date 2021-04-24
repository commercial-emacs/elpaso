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

... But I like quelpa.
    Quelpa is less likely to fail spectacularly, yes, but its slavish copy-paste of MELPA's `package-build`_ code inherits the latter's erroneous versioning.  Quelpa also cannot handle GNU or NonGNU ELPA recipes (or in FSF nomenclature "specs") without changes.

... Where did you put my pre-existing installation of package XYZ?
    I put it in ``~/.emacs.d/elpaso/backups``.

... How do I pin commits, freeze working configurations, do everything that Straight does?
    You don't, I'm afraid.

... How can I edit packages in-place like Straight?
    Come down from the ledge.  Fork-clone the package as you normally would, say in ``/home/kilroy/package``.
    Then add to the list in ``~/.emacs.d/elpaso/recipes/user/recipes``, the entry::

        ;; -*- lisp-data -*-
        (
         (package :url "/home/kilroy/package" :files ("\*.el" "lisp/\*.el"))
        )

    (the recipe will vary), followed by ``M-x elpaso-refresh``, and ``M-x elpaso-install``.

... Why did elpaso unnecessarily fetch a dependency?
    Elpaso can't know a MELPA-presumed package-require such as ``(dash 20210401)`` isn't a real version until it fetches from source.

Some `uninteresting observations`_ about the ELPAs.

.. _Getting started: http://melpa.org/#/getting-started
.. _Issue 2944: https://github.com/melpa/melpa/issues/2944
.. _Advising Functions: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
.. _reimplementing their service: https://github.com/dickmao/shmelpa
.. _quelpa: https://github.com/quelpa/quelpa
.. _package-build: https://github.com/melpa/package-build
.. _uninteresting observations: https://raw.githubusercontent.com/dickmao/elpaso/dev/elpas.txt

.. |build-status|
   image:: https://github.com/dickmao/elpaso/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/elpaso/actions
   :alt: Build Status

.. |---| unicode:: U+02014 .. em dash
   :trim:
