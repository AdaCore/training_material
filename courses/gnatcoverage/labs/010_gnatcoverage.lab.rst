------------------
GNATcoverage Lab
------------------

* Use :toolname:`GNAT Studio` to open source project (:filename:`default.gpr`)

  * Simplistic Point-Of-Sale system that contains:

    * Database of inventory items
    * Mechanism to load a shipment of items
    * Console to capture output
    * Interface to deal with customer purchase/returns

* Build :ada:`Main` to ensure everything is ready for coverage

* Run :ada:`Main` so you can see how it works

  * **Load**

    * Read a text file containing lines in the format ``<item> | <quantity>``
    * Sample text file included in distribution

  * **Print**

    * Print a catalog of current inventory

  * **Sell**

    * Sell an item (specify item and quantity)

  * **Return**

    * Return an item (specify item and quantity)

  * **Quit**

--------------------
Statement Coverage
--------------------

* First we want to do simple Statement Coverage

* Set project coverage type to *Statement*

  .. container:: animate

    * :menu:`Edit` |rightarrow| `:menu:`Project Properties` |rightarrow| menu:`GNATcov`
    * Set both *Coverage Level* values to **stmt** and then :menu:`Save`
  
* Because our application is interactive, we cannot do :menu:`Run All Actions`

  * It assumes :ada:`Main` will terminate on its own

* So we need to perform the steps manually.

  .. container:: animate

    * Under :menu:`Analyze` |rightarrow| :menu:`Coverage` |rightarrow| :menu:`GNATcoverage Source Traces`

  1) Instrument project
  2) Build instrumented project
  3) Run the executable

    * This must be from the command line!
    * If you do not run from the :filename:`obj` directory, you will need to update the trace location in the next step

  4) Generate report

* Just by trying each of the four options, you should end up with 87% statement coverage!

* Try to get close to 100% coverage (only worry about error handlers if you have time)

----------------
MC/DC Coverage
----------------

* Next we try MC/DC Coverage

  * For simple boolean expression (only one subcondition) *covered* will be the same in Decision and MC/DC
  * There is only one multiple subcondition expression in the lab, so we'll skip Decision coverage and go straight to MC/DC

* Set project coverage type to *Statement + MC/DC*

* Perform instrumentation, execution, and report generation as in the previous slide

* Coverage percentage has gone down because we generally hit only one branch of a decision

* Try to get close to 100% coverage

  * Use remaining time to try to reach error handlers
