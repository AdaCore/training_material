=========================
Ada.Characters.Handling
=========================
    
-------------------
Character Queries
-------------------

* Boolean functions whose return is based on the *category* of the character, such as:

  .. container:: latex_environment footnotesize

    .. code:: Ada

     function Is_Control           (Item : Character) return Boolean;
     function Is_Graphic           (Item : Character) return Boolean;
     function Is_Letter            (Item : Character) return Boolean;
     function Is_Lower             (Item : Character) return Boolean;
     function Is_Upper             (Item : Character) return Boolean;
     function Is_Basic             (Item : Character) return Boolean;
     function Is_Digit             (Item : Character) return Boolean;
     function Is_Decimal_Digit     (Item : Character) return Boolean
       renames Is_Digit;
     function Is_Hexadecimal_Digit (Item : Character) return Boolean;
     function Is_Alphanumeric      (Item : Character) return Boolean;

--------------------------
Character Transformation
--------------------------

* Functions to force case 

  .. container:: latex_environment footnotesize

    .. code:: Ada

      function To_Lower (Item : in Character) return Character;
      function To_Upper (Item : in Character) return Character;

* Functions to force case (string version)

  .. container:: latex_environment footnotesize

    .. code:: Ada

      function To_Lower (Item : in String) return String;
      function To_Upper (Item : in String) return String;

* Functions to convert to/from :ada:`Wide_Character` and :ada:`Wide_String`

  .. container:: latex_environment footnotesize

    .. code:: Ada

       function To_Character (Item       : Wide_Character;
                              Substitute : Character := ' ')
                              return Character;
       function To_String (Item       : Wide_String;
                           Substitute : Character := ' ')
                           return String;
       function To_Wide_Character (Item : Character)
                                   return Wide_Character;
       function To_Wide_String (Item : String)
                                return Wide_String;

