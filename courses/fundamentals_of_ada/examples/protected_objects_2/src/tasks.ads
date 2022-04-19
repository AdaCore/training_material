package Tasks is
   task type T is
      entry Start
        (Id : Character; Initial_1, Initial_2 : Integer);
      entry Receive_Message (Delta_1, Delta_2 : Integer);
   end T;

   T1, T2 : T;
end Tasks;
