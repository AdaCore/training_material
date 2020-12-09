:title: Lab 4 - Array Types
:subtitle: The Sun, the Earth, the Moon and one Satellite

The purpose of this exercise is to animate planets on the screen, one planet turning
around the other.


.. figure:: Labs/Solar_System/04_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

Create an enumeration type for various celestial bodies (Sun, Earth, Moon, and
Satellite) and an enumeration for various parameters (X, Y, Radius, Speed, Distance,
and Angle).

Create two array types, one array containing the colors of each body,
and one two-dimensional array for the values of the parameters to store.

Declare one variables of each array type.
Initialize the values of the parameters array with the following semantics:

* :code:`Speed`: angle step (in radian) that has to be added to the star angle at
  each iteration. Use values between :code:`0.01` and :code:`0.1` radian per
  iteration for this exercise.
* :code:`Distance`: distance from the body to the previous one (not used
  by the Sun star). This distance is in pixels, use a value
  between :code:`5` and :code:`50` pixels.
* :code:`Angle`: current angle between the body and the previous body (not used
  by the Sun star). This angle may be initialized at :code:`0`.
* :code:`X` and :code:`Y` represent the position of the object and can be initialized
  at :code:`0`.

Initialize the values of the array variables containing the color of each object.

==========
Question 2
==========

Create an infinite loop that moves the four bodies by one predefined step on every
iteration. Each object is supposed to rotate around the previous one in the 
:code:`Bodies_Enum` enumeration.

Use a :code:`delay until` statement in the loop to control the period of iteration.

Refine the values chosen in the previous question in order to have a smooth movement.
