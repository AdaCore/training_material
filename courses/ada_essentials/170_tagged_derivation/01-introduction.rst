==============
Introduction
==============

---------------------------------------------
Object-Oriented Programming with Tagged Types
---------------------------------------------

* For :ada:`record` types

    .. code:: Ada

       type T is tagged record
       ...

* Child types can add new components (*attributes*)
* Object of a child type can be **substituted** for base type
* Primitive (*method*) can :dfn:`dispatch` **at run-time** depending on the type at call-site
* Types can be **extended** by other packages

    - Conversion and qualification to base type is allowed

* Private data is encapsulated through **privacy**

------------------------------
Tagged Derivation Ada Vs C++
------------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       type T1 is tagged record
         Member1 : Integer;
       end record;

       procedure Attr_F (This : T1);

       type T2 is new T1 with record
         Member2 : Integer;
       end record;

       overriding procedure Attr_F (
            This : T2);
       procedure Attr_F2 (This : T2);

 .. container:: column

    .. code:: C++

       class T1 {
         public:
           int Member1;
           virtual void Attr_F(void);
         };

       class T2 : public T1 {
         public:
           int Member2;
           virtual void Attr_F(void);
           virtual void Attr_F2(void);
         };

