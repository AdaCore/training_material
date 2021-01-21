package body Counter is

  task body Counter_T is
    O_Register  : Datastore.Register_T;
    O_Increment : Integer;
    O_Delay     : Duration;
    Initialized : Boolean := False;
  begin

    loop
      select
        accept Initialize
         (Register   : Datastore.Register_T;
          Value      : Integer;
          Increment  : Integer;
          Delay_Time : Duration) do
          O_Register  := Register;
          O_Increment := Increment;
          O_Delay     := Delay_Time;
          Datastore.Write
           (Register => O_Register,
            Value    => Value);
          Initialized := True;

        end Initialize;
      or
        delay O_Delay;
        if Initialized then
          Datastore.Write
           (Register => O_Register,
            Value    => Datastore.Read (O_Register) + O_Increment);
        end if;
      end select;
    end loop;
  end Counter_T;

end Counter;
