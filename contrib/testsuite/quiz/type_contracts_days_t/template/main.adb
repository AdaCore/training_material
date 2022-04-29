-- Which of the following is a valid subtype predicate?

procedure Main is

   --$ begin question
   type Days_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   function Is_Weekday (D : Days_T) return Boolean is
      (D /= Sun and then D /= Sat);
   --$ end question

   --$ begin cut
   subtype T is Days_T with
      Static_Predicate => T in Sun | Sat;
   --$ end cut
   --$ begin cut
   subtype T is Days_T with Static_Predicate =>
      (if T = Sun or else T = Sat then True else False);
   --$ end cut
   --$ begin cut
   subtype T is Days_T with
      Static_Predicate => not Is_Weekday (T);
   --$ end cut
   --$ begin cut
  subtype T is Days_T with
     Static_Predicate =>
        case T is when Sat | Sun => True,
                  when others => False;
   --$ end cut
begin
   null;
end Main;
