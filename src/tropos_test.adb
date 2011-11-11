with Ada.Text_IO;

with Tropos;
with Tropos.Config;

procedure Tropos_Test is
   Config   : Tropos.Configuration  :=
                Tropos.New_Config ("tropos_test");
   Position : Tropos.Cursor         := Config.First;

   package Configure is new Tropos.Config (Config);
   use Configure;

   Num      : Integer;

begin

   Config.Add ("num", "42");

   while Tropos.Has_Element (Position) loop
      Tropos.Next (Position);
   end loop;
   Num := Get ("num");

   Ada.Text_IO.Put_Line (Num'Img);

end Tropos_Test;
