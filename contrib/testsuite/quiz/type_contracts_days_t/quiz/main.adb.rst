.. code:: Ada

    type Days_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
    function Is_Weekday (D : Days_T) return Boolean is
       (D /= Sun and then D /= Sat);

Which of the following is a valid subtype predicate?

A. | :answermono:`subtype T is Days_T with`
   |    :answermono:`Static_Predicate => T in Sun | Sat;`
B. | ``subtype T is Days_T with Static_Predicate =>``
   |    ``(if T = Sun or else T = Sat then True else False);``
C. | ``subtype T is Days_T with``
   |    ``Static_Predicate => not Is_Weekday (T);``
D. | ``subtype T is Days_T with``
   |    ``Static_Predicate =>``
   |       ``case T is when Sat | Sun => True,``
   |                 ``when others => False;``
