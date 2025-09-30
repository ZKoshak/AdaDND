package Entities is
   type Entity is tagged private;
   type Entity_Ptr is access Entity;
   type Character is new Entity with private;
   type Monster is new Entity with private;
   
   -- Базовые характеристики
   Health: Natural;
   Level: Positive;
   Experience: Natural;
   
   -- Методы
   procedure Update_Entity(Self: in out Entity);
   procedure Render_Entity(Self: in out Entity);
   function Get_Health(Self: Entity) return Natural;
   function Is_Alive(Self: Entity) return Boolean;
   
private
   type Entity is tagged record
      -- Приватные поля
   end record;
end Entities;
