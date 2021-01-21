with Datastore;
package Counter is

  task type Counter_T is
    entry Initialize
     (Register   : Datastore.Register_T;
      Value      : Integer;
      Increment  : Integer;
      Delay_Time : Duration);
  end Counter_T;

end Counter;
