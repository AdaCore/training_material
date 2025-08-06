package P is
   type T is limited private;
   -- Does not need to declare any capability
   -- Declares a constraint: limited
private
   type T is tagged null record;
   -- Declares a capability: tagged
   -- Does not need to declare any constraint
end P;
