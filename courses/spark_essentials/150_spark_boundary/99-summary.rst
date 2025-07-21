=========
Summary
=========

----------------
SPARK Boundary
----------------

* System (hardware, OS) can be modelled in SPARK

  - Using volatile variables and external states
  - With precise volatility properties

* SPARK software boundary defined by aspect/pragma :ada:`SPARK_Mode`

  - Fine-grain integration of SPARK and non-SPARK code is possible

* Integration with other programming languages

  - Easiest between SPARK and Ada
  - Easy between SPARK and C
  - Usually based on C integration for other languages

* Formal verification is based on assumptions

  - Assumptions at the boundary need to be reviewed
