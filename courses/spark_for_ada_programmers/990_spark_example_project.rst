***********************
SPARK Example Project
***********************

.. PRELUDE: BEGIN

.. PRELUDE: ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. PRELUDE: SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. PRELUDE: REQUIRES

.. PRELUDE: PROVIDES

.. PRELUDE: END

------------
Background
------------

* In converting Ada code to SPARK, we typically start with lower-level / utility packages

   - Tend to be more straight-forward
   - Each subprogram tends to have few requirements / interactions

* As a full-scale example, you will take the given Ada package and convert it to SPARK

   - Pre/Post conditions are most important, but you can add `Depends` contracts as well

* The source example is a simplified version of `Ada.Strings.Fixed`

   - Used to manipulate / query character strings
   - Description of each subprogram is supplied in the comments.

---------------------
Lab Setup
---------------------

- Find the :filename:`990_spark_example_project` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane

-------------------------------------
Demonstrating What You Have Learned
-------------------------------------

* Write preconditions to avoid exceptions
* Verify the code

   - You can accept the flow errors with an appropriate justification

   - Correct the code if necessary

* Add postconditions as complete as possible and prove them

   -  You can reformulate the code to make it easier to prove

* To prove `Insert`, `Overwrite`, and `Head` :

   - Assume that `Source` and `New_Item` start at 1.

-------
Hints
-------

* There is one possible solution in the :filename:`answers` folder

   - There are many possible ways to solve this!

* If you get stuck, examine the solution file for suggestions

   - Try to understand the solution rather than copy it!

* Start with the simpler functions first

   - Gets you used to the process
   - `function "*"` is probably the simplest, followed by `Head` and `Tail`
