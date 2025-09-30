package Logging is
   type Log_Level is (DEBUG, INFO, WARNING, ERROR);
   Current_Level : Log_Level := INFO;
   Log_File : Ada.Text_IO.File_Type;
   
   procedure Initialize_Logging(File_Name : String);
   procedure Finalize_Logging;
   
   procedure Log(Message : String; Level : Log_Level := INFO);
   procedure Debug(Message : String);
   procedure Info(Message : String);
   procedure Warning(Message : String);
   procedure Error(Message : String);
   
   procedure Set_Log_Level(Level : Log_Level);
end Logging;
