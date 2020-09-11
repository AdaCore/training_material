--------------------
Type Contracts Lab
--------------------

- Find the :filename:`070_type_contracts` sub-directory in :filename:`source`

   + You can copy it locally, or work with it in-place

- In that directory, open the project :filename:`default.gpr` in :toolname:`GNATstudio`

   + Or, on the command-line, do :command:`gnatstudio -Pdefault.gpr`

- Unfold the source code directory (.) in the project pane 

- You will implement a simplistic calendar program

.. container:: speakernote


   This can be tricky - it took me almost an hour because I didn't make sure my comparison functions were correct.
   Recommend using Ada.Calendar, then you get the comparison functions for free!

--------------
Requirements
--------------

- Finish the implementation of the `important_dates` package

   - Implement type `Calendar_T` to contain a list of events that will always be sorted
   - Implement the following subprograms:

      + `Add_Event` will insert the appropriate event into the calendar
      + `Remove_Event` will remove the event from the calendar
      + `Print_Events` will print the specified number of events occuring on or after the specified date

- There is a `main` program that will test your implementation

   + Do not modify this program
   + So you cannot modify the parts of `important_dates` that `main` depends on

      * But feel free to make other changes as needed

-------
Hints
-------

- Use type predicates to ensure the dates are legal
- Use a type invariant to ensure that the calendar object is always sorted
- Run the executable to see if your implementation works

   + There are some robustness tests that you will need to handle
