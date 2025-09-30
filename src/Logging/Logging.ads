package Logging is
    -- Уровни логирования
    subtype Log_Level is Integer range 0..5;
    DEBUG : constant Log_Level := 5;
    INFO : constant Log_Level := 4;
    WARNING : constant Log_Level := 3;
    ERROR : constant Log_Level := 2;
    CRITICAL : constant Log_Level := 1;
    FATAL : constant Log_Level := 0;
    
    -- Типы и константы
    type Log_File_Type is file of Character;
    Log_File : Log_File_Type;
    Current_Log_Level : Log_Level := DEBUG;
    
    -- Процедуры инициализации
    procedure Initialize_Logging(Filename : String);
    procedure Set_Log_Level(Level : Log_Level);
    procedure Finalize_Logging;
    
    -- Основные процедуры логирования
    procedure Log(Message : String; Level : Log_Level);
    procedure Debug(Message : String);
    procedure Info(Message : String);
    procedure Warning(Message : String);
    procedure Error(Message : String);
    procedure Critical(Message : String);
    procedure Fatal(Message : String);
    
    -- Вспомогательные функции
    function Get_Current_Time return String;
    function Format_Message(Level : Log_Level; Message : String) return String;
end Logging;
