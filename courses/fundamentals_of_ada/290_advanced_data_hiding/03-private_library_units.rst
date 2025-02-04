=======================
Private Library Units
=======================

-------------------------
Child Units and Privacy
-------------------------

* Normally, a child public part cannot view a parent private part

  .. container:: columns

    .. container:: column

      .. code:: Ada

        package Root is
        private
           type T is range 1..10;
        end Root;

    .. container:: column

      .. code:: Ada

        package Root.Child is
          X1 : T; -- illegal
        private
          X2 : T;
        end Root.Child;

* :dfn:`Private child` can view the private part

    + Used for "implementation details"

---------------------------
Importing a Private Child
---------------------------

* A :ada:`private package` can view its **parent** :ada:`private` part
* A private package's usage (*view*) is

   + Restricted to the *Private descendents of their parent*
   + Visible from parent's :ada:`body`
   + Visible from public sibling's :ada:`private` section, and :ada:`body`
   + Visible from private siblings (public, :ada:`private`, :ada:`body`)

  .. container:: columns

    .. container:: column

      .. code:: Ada

        package Root is
        private
          type T is range 1..10;
        end Root;

    .. container:: column

      .. code:: Ada

        private package Root.Child is
          X1 : T;
        private
          X2 : T;
        end Root.Child;

        with Root.Child; -- illegal
        procedure Main is
        begin
           Root.Child.X1 := 10; -- illegal
        end Main;

----------------------------------
Private Children and :ada:`with`
----------------------------------

.. code:: Ada

   private package Root.Child1 is
      type T is range 1 .. 10;
   end Root.Child1;

.. container:: columns

  .. container:: column

    .. container:: latex_environment scriptsize

      *Public package spec cannot* :ada:`with` *a private package*

      .. code:: Ada
         :number-lines: 1

         with Root.Private_Child;
         package Root.Bad_Child is
            Object1 : Root.Private_Child.T;
            procedure Proc2;
         private
            Object2 : Root.Private_Child.T;
         end Root.Bad_Child;

      ``root-bad_child.ads:1:06: error: current unit must also be private descendant of "Root"``

  .. container:: column

    .. container:: latex_environment scriptsize

      *But it can* :ada:`with` *a sibling private package from its body*

      .. code:: Ada

         package Root.Good_Child is
            procedure Proc2;
         end Root.Good_Child;

         with Root.Private_Child;
         package body Root.Good_Child is
            Object1 : Root.Private_Child.T;
            Object2 : Root.Private_Child.T;
            procedure Proc2 is null;
         end Root.Good_Child;

---------------------
:ada:`private with`
---------------------

* The parent and its children can :ada:`private with` a private package

    + From anywhere
    + View given **stays** :ada:`private`

   .. code:: Ada

      private with Root.Child1;
      package Root.Child2 is
         X1 : Root.Child1.T; -- illegal
      private
         X2 : Root.Child1.T;
      end Root.Child2;

* Clients of :ada:`Root.Child2` don't have any visibility on :ada:`Root.Child1`

------------------------------------------------------------
Children "Inherit" From Private Properties of Parent
------------------------------------------------------------

* Private property always refers to the direct parent
* Public children of private packages stay private to the outside world
* Private children of private packages restrain even more the accessibility

.. code:: Ada

   package Root is
   end Root;

   private package Root.Child is
     --  with allowed on Root body
     --  with allowed on Root children
     --  with forbidden outside of Root
   end Root.Child;

   package Root.Child.Grand1 is
     --  with allowed on Root body
     --  with allowed on Root children
     --  with forbidden outside of Root
   end Root.Child.Grand1;

   private package Root.Child.Grand2 is
     --  with allowed on Root.Child body
     --  with allowed on Root.Child children
     --  with forbidden outside of Root.Child
     --  with forbidden on Root
     --  with forbidden on Root children
   end Root.Child1.Grand2;

