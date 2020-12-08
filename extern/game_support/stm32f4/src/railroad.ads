with Screen_Interface;

package Railroad is

   procedure Initialize;
   procedure Spawn_Train;
   procedure Simulation_Step;
   procedure Draw (Init : Boolean := False);
   procedure On_Touch (P : Screen_Interface.Point);
end Railroad;
