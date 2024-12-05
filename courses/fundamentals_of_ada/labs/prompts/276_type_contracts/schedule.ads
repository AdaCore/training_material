--Schedule_Spec
package Schedule is
   subtype Name_T is String (1 .. 10);
   type Days_T is (Mon, Tue, Wed, Thu, Fri, None);
   type Time_T is delta 0.5 range 0.0 .. 23.5;
   type Classes_T is tagged private;
   procedure Add_Class (Classes    : in out Classes_T;
                        Name       :        Name_T;
                        Day        :        Days_T;
                        Start_Time :        Time_T;
                        End_Time   :        Time_T);
   -- Add pre/post condition if appropriate
   procedure Print (Classes : Classes_T);
   function Count (Classes : Classes_T) return Natural;
private
   type Classes_T is tagged null record;
   -- Implement with type invariants/subtype predicates
end Schedule;
