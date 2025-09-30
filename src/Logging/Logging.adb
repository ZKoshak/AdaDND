package body Logging is
   
   procedure Initialize_Logging(File_Name : String) is
   begin
      Ada.Text_IO.Create(Log_File, Ada.Text_IO.Out_File, File_Name);
      if not Ada.Text_IO.Is_Open(Log_File) then
   Put_Line("Ошибка при инициализации логирования!");
   return;
end if;
   end Initialize_Logging;
   
   procedure Finalize_Logging is
   begin
      if Ada.Text_IO.Is_Open(Log_File) then
         Ada.Text_IO.Close(Log_File);
      end if;
   end Finalize_Logging;
   
   procedure Log(Message : String; Level : Log_Level := INFO) is
      Current_Time : constant String := Ada.Calendar.Format(Ada.Calendar.Clock);
   begin
      if Level >= Current_Level then
         declare
            Formatted_Message : constant String := 
               Current_Time & " [" & Log_Level'Image(Level) & "] " & Message;
         begin
            Ada.Text_IO.Put_Line(Formatted_Message);
            Ada.Text_IO.Put_Line(Log_File, Formatted_Message);
         end;
      end if;
   end Log;
   
   procedure Debug(Message : String) is
   begin
      Log(Message, DEBUG);
   end Debug;
   
   procedure Info(Message : String) is
   begin
      Log(Message, INFO);
   end Info;
   
   procedure Warning(Message : String) is
   begin
      Log(Message, WARNING);
   end Warning;
   
   procedure Error(Message : String) is
   begin
      Log(Message, ERROR);
   end Error;
   
   procedure Set_Log_Level(Level : Log_Level) is
   begin
      Current_Level := Level;
   end Set_Log_Level;
   
end Logging;
