=========================
Circular Dependencies
=========================

------------------------------
Handling Cyclic Dependencies
------------------------------

* Elaboration must be linear
* Package declarations cannot depend on each other

   - No linear order is possible

* Which package elaborates first?

.. image:: cyclic_dependencies.png
   :width: 50%
   :align: center

--------------------------------------
Body-Level Cross Dependencies Are OK
--------------------------------------

* The bodies only depend on other packages' declarations
* The declarations are already elaborated by the time the bodies are elaborated

.. image:: mutual_dependencies.svg
   :width: 70%
   :align: center

--------------------------
Resulting Design Problem
--------------------------

* Good design dictates that conceptually distinct types appear in distinct package declarations

   - Separation of concerns
   - High level of *cohesion*

* Not possible if they depend on each other
* One solution is to combine them in one package, even though conceptually distinct

   - Poor software engineering
   - May be only choice, depending on language version

     - Best choice would be to implement both parts in a new package

-------------------------------------------
Circular Dependency in Package Declaration
-------------------------------------------

.. code:: Ada

   with Department; --  Circular dependency
   package Personnel is
     type Employee is private;
     procedure Assign (This : in Employee;
                        To : in out Department.Section);
   private
     type Employee is record
       Assigned_To : Department.Section;
     end record;
   end Personnel;

   with Personnel; --  Circular dependency
   package Department is
     type Section is private;
     procedure Choose_Manager (This : in out Section;
                                Who : in Personnel.Employee);
   [...]
   end Department;
