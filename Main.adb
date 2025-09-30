-- main.adb - точка входа в программу
with Ada.Text_IO; use Ada.Text_IO;
with Game_Core; use Game_Core;
with System_Init; use System_Init;
with UI.Console; use UI.Console;
with Ada.Exceptions;

procedure Main is
   -- Глобальные переменные
   Game_Running : Boolean := True;
   Initialized : Boolean := False;
   Log_File : File_Type;
   
begin
   
   Create(Log_File, Out_File, "game.log");
   begin
     -- Инициализация системы
      Initialize_System;
      Initialize_Game;
      Initialized := True;
      
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
            case Input is
               when 'q' | 'Q' =>
                  Game_Running := False;
                  Put_Line("Выход из игры...");
                  
               when 'h' | 'H' =>
                  Show_Help;
                  
               when 'n' | 'N' =>
                  New_Game;
                  
               when others =>
                  Process_Command(Input);
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
            Error_Msg : String := Exception_Information(E);
         begin
            Put_Line("Ошибка: " & Error_Msg);
            Put_Line(Log_File, "Ошибка: " & Error_Msg);
         end;
   end;
   
finally
   Close(Log_File);
   if Initialized then
      Shutdown_Game;
      Shutdown_System;
   end if;
   Put_Line("Спасибо за игру!");
end Main;
