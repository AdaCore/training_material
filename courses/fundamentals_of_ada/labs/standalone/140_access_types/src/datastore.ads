package Datastore is

  type String_Ptr_T is access String;
  type History_T is array (1 .. 10) of String_Ptr_T;
  type Element_T is tagged record
    History : History_T;
  end record;
  type Reference_T is access all Element_T;
  type Constant_Reference_T is access constant Element_T;

  subtype Index_T is Integer range 1 .. 100;

  function Object
   (Index : Index_T)
    return Reference_T;
  function View
   (Index : Index_T)
    return Constant_Reference_T;

end Datastore;
