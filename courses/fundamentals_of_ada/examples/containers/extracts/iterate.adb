for Student_Count of Student_Per_Day loop
   Put_Line (Integer'Image (Student_Count));
end loop;

for Parcel_Id of Received_Parcels loop
   Put_Line (Parcel_Id);
end loop;

-- We use the cursor to have both key and value
for C in Math_Constants.Iterate loop
   Put_Line
     (To_String (Key (C)) & " = " &
      Float'Image (Element (C)));
end loop;
