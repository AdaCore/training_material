===================================
Detour - 'Image for Complex Types
===================================

------------------
'Image Attribute
------------------

.. admonition:: Language Variant

   Ada 2022

* Previously, we saw the string attribute :ada:`'Image` is provided for scalar types

  * e.g. :ada:`Integer'Image(10+2)` produces the string **" 12"**

* Starting with Ada 2022, the :ada:`Image` attribute can be used for any type

  .. code:: Ada

    with Ada.Text_IO; use Ada.Text_IO;
    procedure Main is
       type Colors_T is (Red, Yellow, Green);
       type Array_T is array (Colors_T) of Boolean;
       Object : Array_T :=
         (Green  => False,
          Yellow => True,
          Red    => True);
    begin
       Put_Line (Object'Image);
    end Main;

  Yields an output of 

  :command:`[TRUE, TRUE, FALSE]`

---------------------------------
Overriding the 'Image Attribute
---------------------------------

.. admonition:: Language Variant

   Ada 2022

* We don't always want to rely on the compiler defining how we print a complex object

* We can define it - by using :ada:`'Image` and attaching a procedure to the :ada:`Put_Image` aspect

.. code:: Ada

   type Colors_T is (Red, Yellow, Green);
   type Array_T is array (Colors_T) of Boolean with
     Put_Image => Array_T_Image;

-------------------------------
Defining the 'Image Attribute
-------------------------------

.. admonition:: Language Variant

   Ada 2022

* Then we need to declare the procedure

   .. code:: Ada

      procedure Array_T_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  :        Array_T);

   * Which uses the :ada:`Ada.Strings.Text_Buffers.Root_Buffer_Type` as an output buffer
   * (No need to go into detail here other than knowing you do :ada:`Output.Put` to add to the buffer)

* And then we define it

   .. code:: Ada

      procedure Array_T_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  :        Array_T) is
      begin
         for Color in Value'Range loop
            Output.Put (Color'Image & "=>" & Value (Color)'Image & ASCII.LF);
         end loop;
      end Array_T_Image;

----------------------------
Using the 'Image Attribute
----------------------------

.. admonition:: Language Variant

   Ada 2022

* Now, when we call :ada:`Image` we get our "pretty-print" version

  .. code:: Ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Types; use Types;
    procedure Main is
       Object : Array_T := (Green  => False,
                            Yellow => True,
                            Red    => True);
    begin
       Put_Line (Object'Image);
    end Main;

  * Generating the following output

    :command:`RED=>TRUE`

    :command:`YELLOW=>TRUE`

    :command:`GREEN=>FALSE`

* Note this redefinition can be used on any type, even the scalars that have always had the attribute

