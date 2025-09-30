package body Logging is
    
    -- Инициализация логирования
    procedure Initialize_Logging(Filename : String) is
    begin
        Create(Log_File, Out_File, Filename);
        Set_Log_Level(DEBUG);
        Log("--- Начало лога ---", INFO);
    end Initialize_Logging;
    
    -- Установка уровня логирования
    procedure Set_Log_Level(Level : Log_Level) is
    begin
        Current_Log_Level := Level;
        Log("Установлен уровень логирования: " & Integer'Image(Level), INFO);
    end Set_Log_Level;
    
    -- Завершение логирования
    procedure Finalize_Logging is
    begin
        if Is_Open(Log_File) then
            Log("--- Конец лога ---", INFO);
            Close(Log_File);
        end if;
    end Finalize_Logging;
    
    -- Основная процедура записи в лог
    procedure Log(Message : String; Level : Log_Level) is
    begin
        if Level >= Current_Log_Level then
            declare
                Formatted_Message : String := Format_Message(Level, Message);
            begin
                Put_Line(Log_File, Formatted_Message);
                -- Опционально: вывод в консоль
                Put_Line(Formatted_Message);
            end;
        end if;
    end Log;
    
    -- Обёртки для разных уровней
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
    
    procedure Critical(Message : String) is
    begin
        Log(Message, CRITICAL);
    end Critical;
    
    procedure Fatal(Message : String) is
    begin
        Log(Message, FATAL);
    end Fatal;
    
    -- Получение текущего времени
    function Get_Current_Time return String is
        use Ada.Calendar;
        Current : Time := Clock;
        Year, Month, Day : Integer;
        Hour, Minute, Second : Integer;
    begin
        Split(Current, Year, Month, Day, Hour, Minute, Second);
        return Year'Image & "-" & Month'Image & "-" & Day'Image & " " &
              Hour'Image & ":" & Minute'Image & ":" & Second'Image;
    end Get_Current_Time;
    
    -- Форматирование сообщения
    function Format_Message(Level : Log_Level; Message : String) return String is
        Level_Name : String;
    begin
        case Level is
            when DEBUG => Level_Name := "DEBUG";
            when INFO => Level_Name := "INFO";
            when WARNING => Level_Name := "WARNING";
            when ERROR => Level_Name := "ERROR";
            when CRITICAL => Level_Name := "CRITICAL";
            when FATAL => Level_Name := "FATAL";
            when others => Level_Name := "UNKNOWN";
        end case;

        return Get_Current_Time & " " & Level_Name & " " & Message;
        end Format_Message;

    -- Обработка исключений
    procedure Handle_Logging_Error(Msg : String) is
    begin
        -- Простая обработка ошибок
        Put_Line("Ошибка логирования: " & Msg);
    end Handle_Logging_Error;

    -- Дополнительные функции
    function Is_Logging_Enabled return Boolean is
    begin
        return Current_Log_Level >= 0;
    end Is_Logging_Enabled;

end Logging;
