--Datastore
package Datastore is
  type Register_T is (One, Two, Three);

  function Read (Register : Register_T) return Integer;
  procedure Write (Register : Register_T;
                   Value    : Integer);
end Datastore;
