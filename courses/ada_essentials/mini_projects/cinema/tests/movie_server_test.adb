with Movie_Servers;
with Logs;

procedure Movie_Server_Test is
   Server : constant Movie_Servers.Movie_Server_Task_T := Movie_Servers.Make_Default;
begin
   Server.Play_Loop (Dir => "resources/movies/space");
   delay 3.0;
   Server.Pause;
   Logs.Put_Line (Logs.Blue, "server is paused...");
   delay 1.5;
   Server.Resume;
   delay 1.5;
   Server.Finish;
end Movie_Server_Test;
