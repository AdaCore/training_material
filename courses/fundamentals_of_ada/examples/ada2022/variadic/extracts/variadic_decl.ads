procedure printf (format : String; opt_param : int)
   with Import, Convention => C_Variadic_1; -- Note the 1 for a single arg
