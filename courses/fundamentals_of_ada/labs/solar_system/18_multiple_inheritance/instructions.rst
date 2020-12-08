:title: Lab 18 - Multiple Inheritance
:subtitle: A true Object-Oriented :code:`Solar_System` 

The purpose of this exercise is to rewrite the previous exercise using OOP.

Remember that privacy is still important!

.. figure:: Labs/Solar_System/05_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

Create a hierarchy of interfaces as follow

* :code:`Orbit_Ref_I` as an interface implementing :code:`Get_X` and
  :code:`Get_Y` (can be used as an orbit reference)
* :code:`Movable_I` as an interface implementing a :code:`Move` procedure 
* :code:`Orbiting_Body_I` as an interface implementing :code:`Orbit_Ref_I`
  and :code:`Movable_I`
* :code:`Still_Body_I` as an interface implementing :code:`Orbit_Ref_I`
* :code:`Solar_System_I` as an interface implementing :code:`Add_Still_Body`
  and :code:`Add_Moving_Body` procedures and :code:`Movable_I`

==========
Question 2
==========

Create a hierarchy of tagged types as follow

* :code:`Body_Base_T` to store a position (:code:`X`, :code:`Y`) and
  implementing :code:`Orbit_Ref_I`
* :code:`Orbiting_Body_T` as a concrete object extending 
* :code:`Body_Base_T` to store :code:`Distance`, :code:`Speed`,
  :code:`Angle` and :code:`Turns_Around` and implementing :code:`Orbiting_Body_I`
* :code:`Still_Body_T` as a concrete object extending 
  :code:`Body_Base_T` and implementing :code:`Still_Body_I`
* :code:`Solar_System_T` as a concrete object implementing :code:`Solar_System_I`
  able to store a vector of moving bodies and a vector of still bodies

==========
Question 3
==========

Add constructor primitives in order to create concrete types
returning a pointer to the newly allocated object

* :code:`Create_Orbiting` returning an access to :code:`Orbiting_Body_T`
* :code:`Create_Still` returning an access to :code:`Still_Body_T`
* :code:`Create_Solar_System` returning an access to :code:`Solar_System_T`

==========
Question 4
==========

In a specific Graphics package extend the capabilities of our object using
decorator design pattern.

* Create an interface :code:`Drawable_I` implementing a
  :code:`Draw` procedure
* Define :code:`Visible_Body_Decorator_T` as an abstract type implementing
  :code:`Drawable_I` and :code:`Still_Body_I`. This type will store graphic
  information.
* Define :code:`Visible_Orbiting_Body_T` extending
  :code:`Visible_Body_Decorator_T` and implementing :code:`Movable_I`
* Define :code:`Visible_Still_Body_T` extending :code:`Visible_Body_Decorator_T`
* Define :code:`Visible_Solar_System_T` implementing :code:`Drawable_I`
  and :code:`Solar_System_I`

==========
Question 5
==========

* Add constructor :code:`Create_Visible` to :code:`Visible_Orbiting_Body_T`
* Add constructor :code:`Create_Visible` to :code:`Visible_Still_Body_T`
* Add constructor :code:`Create_Visible` to :code:`Visible_Solar_System_T`

==========
Question 6
==========

Make it work ! ;)
