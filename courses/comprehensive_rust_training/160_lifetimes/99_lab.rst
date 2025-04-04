============================
Exercise: Protobuf Parsing
============================

------------------------------
Protobuf Parsing Explanation
------------------------------

In this exercise, you will build a parser for the
:url:`protobuf binary encoding <https://protobuf.dev/programming-guides/encoding/>`. Don't
worry, it's simpler than it seems! This illustrates a common parsing
pattern, passing slices of data. The underlying data itself is never
copied.

Fully parsing a protobuf message requires knowing the types of the
fields, indexed by their field numbers. That is typically provided in a
:rust:`proto` file. In this exercise, we'll encode that information into
:rust:`match` statements in functions that get called for each field.

We'll use the following proto:

.. code:: proto

   message PhoneNumber {
     optional string number = 1;
     optional string type = 2;
   }

   message Person {
     optional string name = 1;
     optional int32 id = 2;
     repeated PhoneNumber phones = 3;
   }


------------------------------
More Information on Protobuf
------------------------------

* Messages

   A proto message is encoded as a series of fields, one after the next.
   Each is implemented as a :dfn:`tag` followed by the value. The tag contains a
   field number (e.g., :rust:`2` for the :rust:`id` field of a :rust:`Person` message)
   and a wire type defining how the payload should be determined from the
   byte stream. These are combined into a single integer, as decoded in
   :rust:`unpack_tag` below.

* Varint

   Integers, including the tag, are represented with a variable-length
   encoding called VARINT. Luckily, :rust:`parse_varint` is defined for you
   below.

* Wire Types

   Proto defines several wire types, only two of which are used in this
   exercise.

   * The :rust:`Varint` wire type contains a single varint, and is used to encode proto values of type :rust:`int32` such as :rust:`Person.id`.

   * The :rust:`Len` wire type contains a length expressed as a varint, followed by a payload of that number of bytes. This is used to encode proto values of type :rust:`string` such as :rust:`Person.name`. It is also used to encode proto values containing sub-messages such as :rust:`Person.phones`, where the payload contains an encoding of the sub-message.

----------------
Protobuf Types
----------------

.. container:: source_include 160_lifetimes/src/160_lifetimes.rs :start-after://ANCHOR-types :end-before://ANCHOR-helpers :code:rust

------------------
Protobuf Helpers
------------------

.. container:: source_include 160_lifetimes/src/160_lifetimes.rs :start-after://ANCHOR-helpers :end-before://ANCHOR-parse_field_solution :code:rust

------------------
Protobuf Problem
------------------

The given code also defines callbacks to handle :rust:`Person` and
:rust:`PhoneNumber` fields, and to parse a message into a series of calls to
those callbacks.

What remains for you is to implement the :rust:`parse_field` function and
the :rust:`ProtoMessage` trait for :rust:`Person` and :rust:`PhoneNumber`.

.. code:: rust

   /// Parse a field, returning the remaining bytes
   fn parse_field(data: &[u8]) -> (Field, &[u8]) {
       let (tag, remainder) = parse_varint(data);
       let (field_num, wire_type) = unpack_tag(tag);
       let (fieldvalue, remainder) = match wire_type {
           _ => todo!("Based on the wire type, build a Field, consuming as many bytes as necessary.")
       };
       todo!("Return the field, and any un-consumed bytes.")
   }

.. container:: source_include 160_lifetimes/src/160_lifetimes.rs :start-after://ANCHOR-parse_message :end-before://ANCHOR-traits_solution :code:rust

.. code:: rust

   // TODO: Implement ProtoMessage for Person and PhoneNumber.

-----------------------
Protobuf Main Program
-----------------------

.. container:: source_include 160_lifetimes/src/160_lifetimes.rs :start-after://ANCHOR-main :code:rust

--------------------
Protobuf Solutions
--------------------

.. container:: source_include 160_lifetimes/src/160_lifetimes.rs :start-after://ANCHOR-parse_field_solution :end-before://ANCHOR-parse_message :code:rust

.. container:: source_include 160_lifetimes/src/160_lifetimes.rs :start-after://ANCHOR-traits_solution :end-before://ANCHOR-main :code:rust
