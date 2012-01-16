with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Tropos.Writer is

   ------------------
   -- Write_Config --
   ------------------

   procedure Write_Config
     (Config : Configuration;
      Path   : String)
   is
      procedure Write_Config (Item   : Configuration;
                              Indent : Ada.Text_IO.Positive_Count);

      function Maybe_Quote (Text : String) return String;
      function Escape_Special (Text : String) return String;

      -------------------
      -- Escape_Quotes --
      -------------------

      function Escape_Special (Text : String) return String is
         use Ada.Characters.Latin_1;
         Result   : String (1 .. Text'Length * 2);
         Length   : Natural := 0;
         Got_CRLF : Boolean := False;
      begin
         for I in Text'Range loop
            if Text (I) = '"' then
               Length := Length + 1;
               Result (Length) := '\';
               Length := Length + 1;
               Result (Length) := '"';
            elsif Text (I) = CR then
               if I < Text'Last and then
                 Text (I + 1) = LF
               then
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'n';
                  Got_CRLF := True;
               else
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'r';
               end if;
            elsif Text (I) = LF then
               if Got_CRLF then
                  Got_CRLF := False;
               else
                  Length := Length + 1;
                  Result (Length) := '\';
                  Length := Length + 1;
                  Result (Length) := 'n';
               end if;
            elsif Text (I) = HT then
               Length := Length + 1;
               Result (Length) := ' ';
            else
               Length := Length + 1;
               Result (Length) := Text (I);
            end if;
         end loop;
         return Result (1 .. Length);
      end Escape_Special;

      -----------------
      -- Maybe_Quote --
      -----------------

      function Maybe_Quote (Text : String) return String is
         use Ada.Characters.Handling;
      begin
         if Text'Length = 0 then
            return '"' & '"';
         end if;
         for I in Text'Range loop
            if not Is_Letter (Text (I)) and then
              not Is_Digit (Text (I)) and then
              Text (I) /= '_' and then
              Text (I) /= '.'
            then
               return '"' & Escape_Special (Text) & '"';
            end if;
         end loop;
         return Text;
      end Maybe_Quote;

      ------------------
      -- Write_Config --
      ------------------

      procedure Write_Config (Item   : Configuration;
                              Indent : Ada.Text_IO.Positive_Count)
      is
         use Ada.Text_IO;
      begin
         Set_Col (Indent);
         Put (Item.Config_Name);
         if Child_Count (Item) = 0 then
            --  Put_Line (" = {}");
            New_Line;
         elsif Child_Count (Item) = 1 and then
           Item.Children.Element (1).Child_Count = 0
         then
            Put (" = ");
            Put (Maybe_Quote (Item.Children.Element (1).Config_Name));
            New_Line;
         else
            Put (" = {");
            declare
               New_Indent : constant Positive_Count :=
                 Col + 1;
            begin
               for I in 1 .. Child_Count (Item) loop
                  Write_Config (Item.Children.Element (I).all, New_Indent);
               end loop;
               Set_Col (New_Indent - 2);
               Put_Line ("}");
            end;
         end if;

      end Write_Config;

      File : Ada.Text_IO.File_Type;
   begin
      if Path /= "" then
         Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
         Ada.Text_IO.Set_Output (File);
      end if;

      for I in 1 .. Child_Count (Config) loop
         Write_Config (Config.Children.Element (I).all, 1);
      end loop;

      if Path /= "" then
         Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
         Ada.Text_IO.Close (File);
      end if;

   end Write_Config;

end Tropos.Writer;
