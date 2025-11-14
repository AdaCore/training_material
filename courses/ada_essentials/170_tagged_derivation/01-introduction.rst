==============
Introduction
==============

---------------------------------------------
Object-Oriented Programming with Tagged Types
---------------------------------------------

* Only for :ada:`record` types

  .. code:: Ada

     type T is tagged record
     ...

* Child types can add new components
* Types can be **extended** by other packages

    - Conversion and qualification to base type is allowed

------------------------------
Tagged Derivation Ada Vs C++
------------------------------

.. container:: columns

  .. container:: column

    .. container:: latex_environment scriptsize

      .. code:: Ada

        type Cycle_T is tagged record
          Number_Wheels : Positive;
        end record;

        function Wheels
           (This : Cycle_T)
            return Positive;

        type Unicycle_T is new Cycle_T
        with record
          Seat_Height : Float;
        end record;

        overriding
        function Wheels
          (This : Unicycle_T)
           return Positive is (1);
        function Seat
          (This : Unicycle_T)
           return Float;

  .. container:: column

    .. container:: latex_environment scriptsize

      .. code:: C++

        class Cycle {
          public:
            unsigned NumberWheels;
            virtual unsigned Wheels(void);
          };

        class Unicycle : public Cycle {
          public:
            float SeatHeight;
            virtual unsigned Wheels(void);
            virtual float Seat(void);
          };

