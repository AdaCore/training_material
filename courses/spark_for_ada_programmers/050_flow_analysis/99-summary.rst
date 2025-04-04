=========
Summary
=========

---------------
Flow Analysis
---------------

* Flow analysis builds a Program Dependence Graph
* Flow analysis detects:

  - Interferences between parameters and global variables
  - Read of uninitialized variable
  - Violation of data dependency contracts (:ada:`Global`)

* Flow analysis allows to reach Bronze level
* Flow analysis is imprecise

  - On value-dependent flows
  - On array assignment to index/slice
  - During generation of data dependency contracts
