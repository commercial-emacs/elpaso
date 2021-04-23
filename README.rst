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
 elpaso -- Elisp Package Archive Self Officiator
=================================================

Self-officiating package manager for the emacs text editor.  Like quelpa_.

Elpaso abjures the middle-man role played by package archive operators like
ELPA and MELPA, building packages directly from upstream sources.

::

    M-x elpaso-refresh
    M-x elpaso-install
    M-x elpaso-delete

Install
=======
Clone and ``make install``.

Frequently Asked Questions
==========================

... But I like quelpa.
    Quelpa is less likely to fail spectacularly, yes, but its slavish copy-paste of MELPA's `package-build`_ code inherits the latter's erroneous versioning.  Quelpa also cannot handle GNU or NonGNU ELPA recipes (or in FSF nomenclature "specs") without changes.

... Where did you put the pre-existing installation of package XYZ?
    ``~/.emacs.d/elpaso/backups``.

    One thing I never appreciated about ``package.el`` was its Hippocratic preservation of erstwhile installs in ``~/.emacs.d/elpa``, cluttering the output of ``M-x list-packages``.  My aversion to clutter is approaching Richard-Hendricks-level neurosis.

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
    Many will disdain having to ``elpaso-install`` for changes to take,
    but *separation of concerns* is `real and spectacular`_.

... Why did elpaso unnecessarily fetch a dependency?
    Elpaso can't know a MELPA-assumed dependency such as ``(dash 20210401)`` isn't a real version until it fetches from source.

    I'm completely on board with MELPA's "take it, leave it, or fix it" stance, but they make it all but impossible to purse the third option.  Prove me wrong by fixing `Issue 2944`_, a difficult task without the tight feedback loop that only access to their server would afford.  It's actually easier to shame them into action by `reimplementing their service`_.  It's even easier to raise elpaso's flag of self-officiating freedom!

.. _Getting started: http://melpa.org/#/getting-started
.. _Issue 2944: https://github.com/melpa/melpa/issues/2944
.. _Advising Functions: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
.. _reimplementing their service: https://github.com/dickmao/shmelpa
.. _quelpa: https://github.com/quelpa/quelpa
.. _real and spectacular: https://en.wikipedia.org/wiki/The_Implant
.. _package build: https://github.com/melpa/package-build

.. |build-status|
   image:: https://github.com/dickmao/elpaso/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/elpaso/actions
   :alt: Build Status

.. |---| unicode:: U+02014 .. em dash
   :trim:
