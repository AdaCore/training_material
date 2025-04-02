=========
Summary
=========

------------------------
Advanced Flow Analysis
------------------------

* Flow dependencies can be specified

  - This can be important for security

* Flow analysis detects:

  - Violation of flow dependency contracts (:ada:`Depends`)
  - Inconsistency between data and flow dependency contracts

* Flow analysis is imprecise

  - On value-dependent flows
  - On array assignment to index/slice
