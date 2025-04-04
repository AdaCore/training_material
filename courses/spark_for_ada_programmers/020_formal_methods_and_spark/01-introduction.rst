==============
Introduction
==============

-------------------------
High-Integrity Software
-------------------------

* Also known as (safety- or security- or mission-) :dfn:`critical software`
* Has reliability as the most important requirement

  - More than cost, time-to-market, etc.

* Must be known to be **reliable** before being deployed

  - With **extremely** low failure rates

        + e.g., 1 in 10:superscript:`9` hours (114,080 **years**)

  - Testing alone is insufficient and/or infeasible for such rates

* Is not necessarily safety-critical (no risk of human loss)

  - Satellites
  - Remote exploration vehicles
  - Financial systems

------------------------------------
Developing High-Integrity Software
------------------------------------

* Software quality obtained by a combination of

  - Process

        + Specifications
        + Reviews
        + Testing
        + Others: audits, independence, expertise...

  - Arguments

        + System architecture
        + Use cases
        + Programming language
        + Static code analysis
        + Dynamic code analysis
        + etc...

* Need to comply with a certification regime

  - Process-based or argument-based
  - Independently assessed (avionics, railway) or not (automotive)

