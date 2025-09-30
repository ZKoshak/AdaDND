package World is
   type Map is private;
   type Tile is (Floor, Wall, Door);
--   type Map_Type is array (Integer range <>, Integer range <>) of Character;
--   type World_Ptr is access Map_Type;
   
   -- Основные функции
--   function Generate_Map(width, height: Positive) return World_Ptr;
   function Create_Map(width, height: Natural) return Map;
   procedure Generate_Dungeon(map: in out Map);
   procedure Load_Map(filename: String);
   procedure Save_Map(filename: String);
   function Get_Tile(map: Map; x, y: Integer) return Tile;
   
   -- Работа с координатами
--   function Get_Tile(x, y: Integer) return Character;
--   procedure Set_Tile(x, y: Integer; tile: Character);
end World;

package World is
    type Map is private;
    type Tile is (Floor, Wall, Door);
    
    function Create_Map(width, height: Natural) return Map;
    procedure Generate_Dungeon(map: in out Map);
    function Get_Tile(map: Map; x, y: Integer) return Tile;
end World;
