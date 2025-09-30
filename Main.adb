-- main.adb - точка входа в программу
with Ada.Text_IO; use Ada.Text_IO;
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
            Input : Character := Get_User_Input;
         begin
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
                        Error("Ошибка при обработке команды: " & Exception_Information(E));
                  end;
            end case;
            
            -- Обновление состояния игры
            Update_Game_Loop;
            
            -- Рендеринг
            Render_Game_State;
         end;
      end loop;
      
   exception
      when E : others =>
         declare
            Error_Msg : String := Ada.Exceptions.Exception_Information(E);
         begin
            Error("Произошла ошибка: " & Error_Msg);
         end;
   end;
   
finally
   Info("Завершение работы игры");
   if Initialized then
      Shutdown_Game;
      Shutdown_System;
   end if;
   Finalize_Logging;
   Put_Line("Спасибо за игру!");
end Main;
