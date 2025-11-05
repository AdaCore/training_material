with Datastore;
package Monitor is
  task type Monitor_T is
    entry Initialize (Register   : Datastore.Register_T;
                      Value      : Integer;
                      Increment  : Integer;
                      Delay_Time : Duration);
  end Monitor_T;
end Monitor;
