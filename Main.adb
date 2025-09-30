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
begin
   
   -- Инициализация логирования
   Initialize_Logging("game.log");
   Set_Log_Level(DEBUG);
   
   -- Проверка успешности инициализации логирования
   if not Ada.Text_IO.Is_Open(Log_File) then
      Error("Не удалось открыть файл лога!");
      return;
   end if;
   
   -- Основной блок программы
   begin
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
            -- Проверка на пустой ввод
            if Input = ASCII.NUL then
               Debug("Пустой ввод получен");
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
      
   exception
      when E : others =>
         declare
            Error_Msg : String := Ada.Exceptions.Exception_Information(E);
         begin
            Error("Критическая ошибка в главном цикле: " & Error_Msg);
            Error("Состояние игры: " & Game_State'Image(Current_State));
         end;
   end;
   
finally
   Info("Завершение работы игры");
   begin
      if Initialized then
         Shutdown_Game;
         Shutdown_System;
      end if;
   exception
      when E : others =>
         Error("Ошибка при завершении: " & Exception_Information(E));
   end;
   Finalize_Logging;
   Put_Line("Спасибо за игру!");
end Main;
