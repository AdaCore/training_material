=====
Lab
=====

--------------------
A Very Simple Rule
--------------------

This lab is a hands-on walkthrough of creating your own LKQL rule for use with :toolname:`GNATcheck`.
You can use any text editor to create this rule file.

Our "imaginary" coding standard requires no abstract types in the code, so
we just want to flag any abstract type when we find one.

.. tip::

  The lab is a step-by-step guide. Many of the steps have hints in 
  addition to solutions. If the page has empty space, pressing
  :menu:`Page Down` will usually get you that extra help!

-----------------
Example Project
-----------------

* Copy the :filename:`introduction_to_lkql` folder from the course materials location

* Contents of the folder:

  * :filename:`default.gpr` - project file
  * :filename:`src` - source directory

    * Contains some source code that happens to use abstract types

---------------------------
Step 1 - Create Rule File
---------------------------

.. container:: animate 1-

  * Create rule **flag_abstract_types**

    * In file :filename:`flag_abstract_types.lkql`
    * Use :lkql:`@check` annotation

  * For now, just return :lkqk:`true`

    * This should flag **every** node as a violation

.. container:: animate 2-

  .. code:: lkql

    @check
    fun flag_abstract_types(node) =
        true

  * Run the rule using :toolname:`GNATcheck` and specify

    * Directory containing the rule file
    * Name of rule

.. container:: animate 3-

  :command:`gnatcheck -P default.gpr --rules-dir=. -r flag_abstract_types`

  *If you do not get a lot of rule violations, you did it wrong!*

  .. code:: error

    loggers.adb:1:01: rule violation: flag_abstract_types
    loggers.adb:1:08: rule violation: flag_abstract_types
    loggers.adb:1:16: rule violation: flag_abstract_types
    loggers.adb:1:17: rule violation: flag_abstract_types
    ...

---------------------------
Step 2 - Filter For Types
---------------------------

.. container:: animate 1-

  * Now, only return :lkql:`true` if the node is a type declaration

.. container:: animate 2-

  .. code:: lkql

    @check
    fun flag_abstract_types(node) =
        node is TypeDecl

  *Node types like* :lkql:`TypeDecl` *are in the LKQL API section of the documentation*

  https://docs.adacore.com/live/wave/lkql/html/gnatcheck_rm/gnatcheck_rm/lkql_language_reference.html#node-types

  * Running the command now generates a lot fewer messages

.. container:: animate 3-

  .. code:: error

    loggers.ads:3:09: rule violation: flag_abstract_types
    loggers.ads:4:09: rule violation: flag_abstract_types
    loggers.ads:11:09: rule violation: flag_abstract_types
    loggers.ads:18:09: rule violation: flag_abstract_types
    ...

------------------------------
Step 3 - Find Abstract Types
------------------------------

.. container:: animate 1-

  * Now, only return :lkql:`true` if the type declaration has :ada:`abstract`

  * Hint: :lkql:`TypeDef` is derived from :lkql:`BaseTypeDecl`

.. container:: animate 2-

  * :lkql:`BaseTypeDecl` has many fields and properties

    * So :lkql:`TypeDecl` inherits all of them

  .. code:: lkql

    @check
    fun flag_abstract_types(node) =
        node is TypeDecl and
        node.p_is_abstract_type()

  * Running the command now generates messages as appropriate

.. container:: animate 3-

  .. code:: error

    loggers.ads:3:09: rule violation: flag_abstract_types
    loggers.ads:11:09: rule violation: flag_abstract_types
    loggers.ads:18:09: rule violation: flag_abstract_types
    loggers.ads:22:09: rule violation: flag_abstract_types
    loggers.ads:57:09: rule violation: flag_abstract_types
    loggers.ads:58:09: rule violation: flag_abstract_types
    loggers.ads:60:09: rule violation: flag_abstract_types
    loggers.ads:62:09: rule violation: flag_abstract_types

-------------------------
Step 4 - Better Message
-------------------------

.. container:: animate 1-

  * The current error message is not very friendly

  * Update the rule message to be more helpful

.. container:: animate 2-

  .. code:: lkql

    @check(message="Abstract type found")
    fun flag_abstract_types(node) =
        node is TypeDecl and
        node.p_is_abstract_type()

  * Running the command now generates the new messages

.. container:: animate 3-

  .. code:: error

    loggers.ads:3:09: rule violation: Abstract type found
    loggers.ads:11:09: rule violation: Abstract type found
    loggers.ads:18:09: rule violation: Abstract type found
    loggers.ads:22:09: rule violation: Abstract type found
    loggers.ads:57:09: rule violation: Abstract type found
    loggers.ads:58:09: rule violation: Abstract type found
    loggers.ads:60:09: rule violation: Abstract type found
    loggers.ads:62:09: rule violation: Abstract type found

-------------------------------------
Extra Credit - Removing Duplication
-------------------------------------

.. container:: animate 1-

  * Note that :filename:`actors.ads` flags the same type twice

    * Public view

    .. code:: ada
      :number-lines: 27

      package Actors is
         type Actor is abstract tagged private;
         type Actor_Ref is access all Actor'Class;

    * Private declaration

    .. code:: ada
      :number-lines: 38

      private
         type Actor is abstract tagged record
            Live : Boolean := True;

  * Update the rule to skip the public view

.. container:: animate 2-

  .. code:: lkql

    @check(message="Abstract type found")
    fun flag_abstract_types(node) =
        node is TypeDecl and
        node.p_is_abstract_type() and
        node.p_private_completion() == null

  * Running the command now generates one less message

.. container:: animate 3-

  .. code:: error

    loggers.ads:57:09: rule violation: Abstract type found
    loggers.ads:58:09: rule violation: Abstract type found
    loggers.ads:60:09: rule violation: Abstract type found
    loggers.ads:62:09: rule violation: Abstract type found
