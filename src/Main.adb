-- main.adb - точка входа в программу
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;
with Game_Core; use Game_Core;
with System_Init; use System_Init;
with UI.Console; use UI.Console;
with Logging; use Logging;

procedure Main is
   -- Глобальные переменные
   Game_Running : Boolean := True;
   Initialized : Boolean := False;
   Command_Error_Count : Natural := 0;
   MAX_COMMAND_ERRORS : constant Natural := 5;
begin
   
   -- Инициализация логирования
   Initialize_Logging("game.log");
   Set_Log_Level(DEBUG);
   
   -- Проверка успешности инициализации логирования
   if not Ada.Text_IO.Is_Open(Log_File) then
      Error("Не удалось открыть файл лога!");
      return;
   end if;
   
   -- Проверка существования и корректности флагов инициализации
   if not System_Initialized'Exists or not Game_Initialized'Exists or 
      not System_Initialized or not Game_Initialized then
      Error("Флагов инициализации не существует или они имеют значение False!");
      return;
   end if;
   
   -- Основной блок программы
   begin
      -- Проверка инициализации консоли
      if not Console_Initialized then
         Error("Консоль не инициализирована!");
         return;
      end if;
      
      -- Инициализация системы
      Initialize_System;
      Initialize_Game;
      
      -- Проверка успешности инициализации
      if not System_Initialized or not Game_Initialized then
         Error("Ошибка при инициализации системы или игры!");
         return;
      end if;
      
      Initialized := True;
      
      -- Логирование успешного старта
      Info("Игра успешно инициализирована");
      
      -- Вывод приветственного сообщения
      Clear_Screen;
      Put_Line("Добро пожаловать в AdaDND!");
      Put_Line("--------------------------");
      Put_Line("Нажмите Enter для начала...");
      Skip_Line;
      
      -- Основной игровой цикл
      while Game_Running loop
         declare
            Start_Time : constant Time := Clock;
            Input : Character := Get_User_Input;
         begin
            -- Проверка времени ожидания ввода
            declare
               Input_Time : constant Time := Clock;
               Input_Delay : constant Duration := Input_Time - Start_Time;
            begin
               if Input_Delay > 5.0 then
                  Warning("Длительное ожидание ввода: " & Duration'Image(Input_Delay) & " секунд");
               end if;
            end;
            
            -- Проверка на пустой и EOF ввод
            if Input = ASCII.NUL then
               Debug("Пустой ввод получен");
               goto Continue_Loop;
            elsif Input = EOF then
               Warning("Обнаружен конец файла при вводе");
               Game_Running := False;  -- Добавить принудительное завершение
               goto Continue_Loop;
            end if;
            
            Debug("Получен ввод: " & Input);
            
            case Input is
               when 'q' | 'Q' =>
                  Game_Running := False;
                  Info("Получен запрос на выход");
                  
               when 'h' | 'H' =>
                  Show_Help;
                  Info("Вызвана справка");
                  
               when 'n' | 'N' =>
                  New_Game;
                  Info("Начата новая игра");
                  
               when others =>
                  begin
                     Process_Command(Input);
                  exception
                     when E : others =>
                        Error("Ошибка при обработке команды '" & Input & "': " & Exception_Information(E));
                        Debug("Последняя обработанная команда: " & Last_Command);
                        if Input in Valid_Commands then
                           Warning("Команда валидна, но произошла ошибка при обработке");
                        else
                           Warning("Невалидная команда введена");
                        end if;
                        -- Счётчик ошибок
                        Command_Error_Count := Command_Error_Count + 1;
                        if Command_Error_Count > MAX_COMMAND_ERRORS then
                            Error("Превышено допустимое количество ошибок обработки команд");
                            Game_Running := False;
                        end if;
                  end;
            end case;
            
            -- Обновление состояния игры
            Update_Game_Loop;
            
            -- Рендеринг
            Render_Game_State;
            
            declare
               End_Time : constant Time := Clock;
               Elapsed : constant Duration := End_Time - Start_Time;
            begin
               Debug("Цикл выполнен за " & Duration'Image(Elapsed) & " секунд");
            end;
            
            <<Continue_Loop>>;
         end;
      end loop;
      
      Info("Игровой цикл завершен");
      
   exception
      when E : others =>
         declare
            Error_Msg : String := Ada.Exceptions.Exception_Information(E);
         begin
            Error("Критическая ошибка в главном цикле: " & Error_Msg, CRITICAL);
            Error("Состояние игры: " & Game_State'Image(Current_State));
         end;
   end;
   
finally
   Info("Начинается процесс завершения работы");
   begin
      if Initialized then
         begin
            Shutdown_Game;
         exception
            when E : others =>
               Error("Ошибка при завершении игры: " & Exception_Information(E));
         end;
         
         begin
            Shutdown_System;
         exception
            when E : others =>
               Error("Ошибка при завершении системы: " & Exception_Information(E));
         end;
         
         -- Проверка освобождения ресурсов
         if Resources_Not_Freed then
            Warning("Не все ресурсы были корректно освобождены!");
            -- Добавить дополнительную информацию
            Debug("Оставшиеся ресурсы: " & Get_Remaining_Resources_Info);
            Force_Free_Resources;
         end if;
      end if;
      
      begin
         Finalize_Logging;
      exception
         when E : others =>
            Error("Ошибка при завершении логирования: " & Exception_Information(E));
      end;
      
      Put_Line("Спасибо за игру!");
      
   exception
      when E : others =>
         Error("Критическая ошибка при завершении: " & Exception_Information(E), CRITICAL);
   end;
end Main;
