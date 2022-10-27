-- xxx2 are objects with the exact same content
pragma Assert (Student_Per_Day = Student_Per_Day2);
pragma Assert (Received_Parcels = Received_Parcels2);
pragma Assert (Math_Constants = Math_Constants2);

-- After changing the content, equality does not hold
Student_Per_Day.Append (10);
Received_Parcels.Insert ("Chronopost 13214GUU-035");
Math_Constants.Insert (To_Unbounded_String ("G"), 9.8);

pragma Assert (Student_Per_Day /= Student_Per_Day2);
pragma Assert (Received_Parcels /= Received_Parcels2);
pragma Assert (Math_Constants /= Math_Constants2);
