-- main.adb - точка входа в программу
with Ada.Text_IO; use Ada.Text_IO;
with Game_Core; use Game_Core;
with System_Init; use System_Init;
with UI.Console; use UI.Console;

procedure Main is
   -- Глобальные переменные
   Game_Running : Boolean := True;
   
begin
   -- Инициализация системы
   Initialize_System;
   
   -- Инициализация игры
   Initialize_Game;
   
   -- Вывод приветственного сообщения
   Clear_Screen;
   Put_Line("Добро пожаловать в AdaDND!");
   Put_Line("--------------------------");
   Put_Line("Нажмите Enter для начала...");
   Skip_Line;
   
   -- Основной игровой цикл
   while Game_Running loop
      -- Обработка событий
      case Get_User_Input loop
         when 'q' | 'Q' =>
            Game_Running := False;
            Put_Line("Выход из игры...");
            
         when 'h' | 'H' =>
            Show_Help;
            
         when 'n' | 'N' =>
            New_Game;
            
         when others =>
            Process_Command(Get_User_Input);
      end case;
      
      -- Обновление состояния игры
      Update_Game_Loop;
      
      -- Рендеринг
      Render_Game_State;
   end loop;
   
   -- Завершение игры
   Shutdown_Game;
   Shutdown_System;
   Put_Line("Спасибо за игру!");
   
exception
   when others =>
      Put_Line("Произошла ошибка. Завершение игры...");
      Shutdown_Game;
      Shutdown_System;
end Main;
