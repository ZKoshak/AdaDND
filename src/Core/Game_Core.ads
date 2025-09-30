package Game_Core is
   -- Основные типы данных
   type Game_State is (Running, Paused, Exiting);
   type Game_Mode is (Singleplayer, Multiplayer);
   
   -- Глобальные переменные
   Current_State: Game_State := Running;
   Current_Mode: Game_Mode := Singleplayer;
   
   -- Основные процедуры
   procedure Initialize_Game;
   procedure Main_Game_Loop;
   procedure Update_Game_Loop;
   procedure Shutdown_Game;
   
   -- Управление состоянием
   procedure Set_Game_State(new_state: Game_State);
   function Get_Game_State return Game_State;
   procedure Set_Game_Mode(new_mode: Game_Mode);
end Game_Core;
