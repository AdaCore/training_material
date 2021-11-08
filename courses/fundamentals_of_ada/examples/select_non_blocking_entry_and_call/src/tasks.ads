package Tasks is

   task T is
      entry Start;
      entry Receive_Message (V : String);
      entry Stop;
   end T;

end Tasks;
