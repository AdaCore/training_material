=========
Summary
=========

-------------------------------------
Contract-Based Programming Benefits
-------------------------------------

* Facilitates building software with reliability built-in

   - Software cannot work well unless "well" is carefully defined
   - Clarifies design by defining requirements/guarantees

* Enhances readability and understandability

   - Specification contains explicitly expressed properties of code

* Improves testability but also likelihood of passing!
* Aids in debugging
* Facilitates tool-based analysis

   - Compiler checks conformance to requirements
   - Static analyzers (e.g., SPARK, GNAT Static Analysis Suite) can verify explicit precondition and postconditions

---------
Summary
---------

* Based on viewing source code as clients and suppliers with enforced requirements and guarantees
* No run-time penalties unless enforced
* OOP introduces the tricky issues

   - Inheritance of preconditions and postconditions, for example

* Note that pre/postconditions can be used on concurrency constructs too

 .. list-table::
   :header-rows: 1
   :stub-columns: 1
   :width: 90%

  * -

    - Clients
    - Suppliers

  * - Preconditions

    - Requirement
    - Guarantee

  * - Postconditions

    - Guarantee
    - Requirement
