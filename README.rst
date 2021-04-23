|build-status|

  Bob Slydell: So you physically take the specs from the customer?

  Tom Smykowski: Well... no. My secretary does that, or the fax.

  Bob Porter: So then you must physically bring them to the software people?

  Tom Smykowski: Well. No. Yeah, I mean sometimes!

  |---| *Office Space* (1999)
.. |---| unicode:: U+02014 .. em dash
   :trim:

This package aims to Do the Right Thing by advising ``package-installed-p``
to disregard the MELPA-imposed version in ``PACKAGE-pkg.el``, and
instead refer directly to the Version header in package source files.

To ensure the user has acknowledged the risks of `Advising Functions`_, he
must explicitly insert into his ``.emacs``:

::

(elpaso-activate)

Rationale: ``package-install`` generally will not update a bumped package dependency
because MELPA's timestamp versioning is incompatible with the semantic version numbers
in ``Package-Requires`` clauses (the Schism).

The particulars of the Schism are expatiated in uninteresting detail in `Issue 2944`_.

.. |build-status|
   image:: https://github.com/dickmao/elpaso/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/elpaso/actions
   :alt: Build Status

Install
=======
Clone and ``make install``.

.. _Getting started: http://melpa.org/#/getting-started
.. _Issue 2944: https://github.com/melpa/melpa/issues/2944
.. _Advising Functions: https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
