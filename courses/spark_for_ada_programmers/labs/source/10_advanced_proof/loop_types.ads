with SPARK.Containers.Formal.Doubly_Linked_Lists;
with SPARK.Containers.Formal.Vectors;

package Loop_Types is

   subtype Index_T is Positive range 1 .. 1000;
   subtype Opt_Index_T is Natural range 0 .. 1000;
   subtype Component_T is Natural;

   package Vectors is new SPARK.Containers.Formal.Vectors (Index_T, Component_T);

   package Lists is new SPARK.Containers.Formal.Doubly_Linked_Lists (Component_T);

end Loop_Types;
