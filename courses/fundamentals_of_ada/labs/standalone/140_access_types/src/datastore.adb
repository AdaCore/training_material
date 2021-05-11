package body Datastore is

  type Array_T is array (Index_T) of aliased Element_T;
  Global_Data : aliased Array_T;

  function Object
   (Index : Index_T)
    return Reference_T is (Global_Data (Index)'Access);

  function View
   (Index : Index_T)
    return Constant_Reference_T is (Global_Data (Index)'Access);

end Datastore;
