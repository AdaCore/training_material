========================
Conditional Statements
========================

--------------------------
If-then-elsif Statements
--------------------------

* Sequential choice with alternatives
* Avoids :ada:`if` nesting
* :ada:`elsif` alternatives, tested in textual order
* :ada:`else` part still optional

**else if**

   .. code:: Ada

     if Valve (N) /= Closed then
        Isolate (Valve (N));
        Failure (Valve (N));
     else
        if System = Off then
           Failure (Valve (N));
        end if;
     end if;

**elsif**

  .. code:: Ada

    if Valve (N) /= Closed then
       Isolate (Valve (N));
       Failure (Valve (N));
    elsif System = Off then
       Failure (Valve (N));
    end if;

-----------------
Case Statements
-----------------

* Exclusionary choice among alternatives

.. code:: Ada

   type Directions is  (Forward, Backward, Left, Right);
   Direction : Directions;
   ...
   case Direction is
     when Forward =>
       Set_Mode (Forward);
       Move (1);
     when Backward =>
       Set_Mode (Backup);
       Move (-1);
     when Left =>
       Turn (1);
     when Right =>
       Turn (-1);
   end case;

.. note::

   No fall-through between cases
