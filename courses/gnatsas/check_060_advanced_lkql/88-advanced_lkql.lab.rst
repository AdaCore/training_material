=====
Lab
=====

------------------------
Advanced Rule Building
------------------------

In this lab we, want a rule to check that nested subprograms
are "relatively" small as part of our coding standard.

Again, we are using a "unit rule" because we only need to examine
subprograms - no need to run the rule for every node.

-----------------
Example Project
-----------------

* Copy the :filename:`advanced_lkql` folder from the course materials location

* Contents of the folder:

  * :filename:`default.gpr` - project file
  * :filename:`src` - source directory

    * Contains a trivial collection of subprograms and nested subprograms

---------------------------
Step 1 - Create Rule File
---------------------------

.. container:: animate 1-

  * Create rule :lkql:`long_nested_subprograms`

    * In file :filename:`long_nested_subprograms.lkql`
    * Use :lkql:`@unit_check` annotation

  * Our first step is to flag every subprogram in the unit

  * Rule should use

    * List comprehension
    * Iteration over results of a query
    * Name of subprogram in violation message

.. container:: animate 2-

  .. code:: lkql
    :font-size: footnotesize

    @unit_check
    fun long_nested_subprograms(unit) =
        [
            {message: "subprogram name: " &
                      img(subp.p_defining_name().p_fully_qualified_name()),
             loc: subp}
            for subp in
                (from unit.root
                    select BasicDecl(p_is_subprogram(): true))
        ]

  *Run rule to see behavior*

.. container:: animate 3-

  :command:`gnatcheck -P default.gpr --rules-dir=. --rule long_nested_subprograms`

  .. code:: error
    :font-size: tiny

    advanced.adb:3:04: rule violation: subprogram name: "Advanced.Visible1"
    advanced.adb:4:07: rule violation: subprogram name: "Advanced.Visible1.Nested1"
    advanced.adb:18:07: rule violation: subprogram name: "Advanced.Visible1.Nested2"
    advanced.adb:26:04: rule violation: subprogram name: "Advanced.Visible2"
    advanced.adb:27:07: rule violation: subprogram name: "Advanced.Visible2.Nested1"
    ...

  *Should get 34 total 'violations'*

--------------------------------------
Step 2 - Focus on Nested Subprograms
--------------------------------------

.. container:: animate 1-

  * There is already a rule to count lines in a subprogram (:lkql:`maximum_subprogram_lines`)
  * We want to focus on nested subprograms 

    * We need to see if any of the subprogram parents is a subprogram
    * Create a helper function to search the subprogram parents for :lkql:`SubpBody`

.. container:: animate 2-

  .. code:: lkql
    :font-size: footnotesize

    fun enclosing_subp(n) =
       from n through parent select first SubpBody

  * Now add this function to our list comprehension as a filter

.. container:: animate 3-

  .. code:: lkql
    :font-size: footnotesize

    @unit_check
    fun long_nested_subprograms(unit) =
        [
            {message: "subprogram name: " &
                      img(subp.p_defining_name().p_fully_qualified_name()),
             loc: subp}
            for subp in
                (from unit.root select BasicDecl(p_is_subprogram(): true))
                if enclosing_subp(subp) != null
        ]

  *Run rule to see behavior*

.. container:: animate 4-

  .. code:: error
    :font-size: tiny

    advanced.adb:4:07: rule violation: subprogram name: "Advanced.Visible1.Nested1"
    advanced.adb:18:07: rule violation: subprogram name: "Advanced.Visible1.Nested2"
    advanced.adb:27:07: rule violation: subprogram name: "Advanced.Visible2.Nested1"
    advanced.adb:28:10: rule violation: subprogram name: "Advanced.Visible2.Nested1.Nesteda"
    advanced.adb:32:10: rule violation: subprogram name: "Advanced.Visible2.Nested1.Nestedb"
    ...

  * Down to 24 'violoations'
  * None of the top-level subprograms are listed

------------------------------------------
Step 3 - Count the Lines in a Subprogram
------------------------------------------

.. container:: animate 1-

  * We want to count the lines of code in our nested subprogram
  * Helper functions in :filename:`metrics.lkql` do a lot of those calculations

    * File resides in :toolname:`GNATcheck` installation directory
    * We want :lkql:`logical_slocs`

  * Update our query to only include nested subprogram with more than 5 lines of code

    * Don't forge to add the external reference to :lkql:`metrics`

.. container:: animate 2-

  .. code:: lkql
    :font-size: footnotesize

    if enclosing_subp(subp) != null and metrics.logical_slocs(subp) > 5

  * This is the only logic line that needs to be updated

    * But we need to add :lkql:`import metrics` to the top of the file

  *Run rule to see behavior*

.. container:: animate 3-

  .. code:: error
    :font-size: tiny

    advanced.adb:4:07: rule violation: subprogram name: "Advanced.Visible1.Nested1"
    advanced.adb:18:07: rule violation: subprogram name: "Advanced.Visible1.Nested2"
    advanced.adb:27:07: rule violation: subprogram name: "Advanced.Visible2.Nested1"
    advanced.adb:28:10: rule violation: subprogram name: "Advanced.Visible2.Nested1.Nesteda"
    advanced.adb:32:10: rule violation: subprogram name: "Advanced.Visible2.Nested1.Nestedb"
    ...

  * Down to 11 'violoations'
  * Note that higher level subprograms are more likely to get flagged

    * Because their lines of code include those in the nested subprograms

-----------------------------------
Step 4 - Why Only in Subprograms?
-----------------------------------

.. container:: animate 1-

  * Our rule only checks subprograms nested in other subprograms

    * But what about subprograms in protected objects? or tasks?

  * Delivered helper file :filename:`stdlib.lkql` has a lot of these queries

    * :lkql:`has_local_scope` expands our subprogram parent into something more broad

  * Update the rule to use the :lkql:`stdlib` query and run the rule

.. container:: animate 2-

  .. code:: lkql
    :font-size: footnotesize

    if stdlib.has_local_scope(subp) and metrics.logical_slocs(subp) > 5

  * Don't forget to add :lkql:`import stdlib` to the top of the file
  * (You could also remove :lkql:`enclosing_subp`, but it's not necessary)

  .. code:: error
    :font-size: tiny

    ...
    advanced.adb:97:10: rule violation: subprogram name: "Advanced.Visible3.Nested2.Nesteda"
    advanced.adb:109:10: rule violation: subprogram name: "Advanced.Visible3.Nested2.Nestedb"
    advanced.adb:134:07: rule violation: subprogram name: "Advanced.Protected_Object.Set"

  * Notice the subprogram inside :ada:`Protected_Object` is now included

------------------------------------------------------
Extra Credit - Different Values for Different Depths
------------------------------------------------------

* What if we wanted to enforce smaller limits for deeply nested subprograms?

* Now we need a function to count the depth of the subprogram

  * If a parent is a subprogram, add one to the count

* We can use the :lkql:`reduce` function to count the appropriate parents

  * Use list comprehension to build a list of matching nodes

    * Append **1** to the list if the node matches

  * Use a function to accumulate the list elements
  * Set the initial value of the :lkql:`reduce` accumulator to 0

-------------------------------------------------
Implement Different Values for Different Depths
-------------------------------------------------

.. container:: animate 1-

  * Create a function to add two numbers

.. container:: animate 2-

  .. code:: lkql
    :font-size: footnotesize

    fun add(x, y) = x + y

  * Create a function that uses :lkql:`reduce` to count appropriate parents

.. container:: animate 3-

  .. code:: lkql
    :font-size: footnotesize

    @memoized
    fun count_depth(n) =
        reduce(
            [1 for p in n.parents(true) if p is SubpBody],
            add,
            0
        )

  *We use* :lkql:`@memoized` *because we may use this function*
  *multiple times on the same node*

  * Update rule to use different line counts for different depths

    * Try 10 for depth 2, 8 for depth 3, and 6 for depth 4

.. container:: animate 4-

  .. code:: lkql
    :font-size: footnotesize

    if stdlib.has_local_scope(subp) and
        ( (count_depth(subp) == 4 and metrics.logical_slocs(subp) > 6) or
          (count_depth(subp) == 3 and metrics.logical_slocs(subp) > 8) or
          (count_depth(subp) == 2 and metrics.logical_slocs(subp) > 10))
>>>>>>> 4c8701de (Add advanced LKQL lab)
