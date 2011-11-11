with Ada.Directories;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Tropos.Reader.Parser;

package body Tropos.Reader is

   function Parse_Config return Configuration;

   ------------------
   -- Parse_Config --
   ------------------

   function Parse_Config return Configuration is
      use Tropos.Reader.Parser;

      Result : Configuration;
      Have_Body : Boolean := Tok = Tok_Open_Brace;
   begin
      if Tok = Tok_Name or else Tok = Tok_Open_Brace then
         if Tok = Tok_Name then
            Result := New_Config (Tok_Text);
            Next;
            if Tok = Tok_Equal then
               Next;
               Have_Body := True;
            end if;
         else
            Result := New_Config ("");
         end if;

         if Have_Body then
            if Tok = Tok_Name then
               Add (Result, New_Config (Tok_Text));
               Next;
            elsif Tok = Tok_Open_Brace then
               Next;
               while not End_Of_File and then
                  Tok /= Tok_Close_Brace
               loop
                  Add (Result, Parse_Config);
               end loop;
               if End_Of_File then
                  Error ("missing '}' at end of file");
               else
                  Next;
               end if;
            else
               Error ("expected name or '{'");
            end if;
         end if;
      else
         Error ("missing name");
         Result := Empty_Config;
      end if;
      return Result;
   end Parse_Config;

   -----------------
   -- Read_Config --
   -----------------

   function Read_Config (Path : String) return Configuration is
      Result : Configuration :=
        New_Config (Ada.Directories.Base_Name
                    (Ada.Directories.Simple_Name (Path)));
   begin
      Tropos.Reader.Parser.Open (Path);
      while not Tropos.Reader.Parser.End_Of_File loop
         declare
            Child : constant Configuration := Parse_Config;
         begin
            Add (Result, Child);
         end;
      end loop;
      return Result;
   exception
      when Tropos.Reader.Parser.Parse_Error =>
         return Empty_Config;
   end Read_Config;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config (Path : String;
                          Configure : not null access procedure
                            (Config : Configuration))
   is
      Result : constant Configuration := Read_Config (Path);
      It     : Cursor := First (Result);
   begin
      while Has_Element (It) loop
         Configure (Element (It));
         Next (It);
      end loop;
   end Read_Config;

   -----------------
   -- Read_Config --
   -----------------

   function Read_Config (Path      : String;
                         Extension : String)
                         return Configuration
   is
      Result : Configuration :=
        New_Config (Ada.Directories.Base_Name
                    (Ada.Directories.Simple_Name (Path)));

      procedure Call_Reader
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      procedure Recurse
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      -----------------
      -- Call_Reader --
      -----------------

      procedure Call_Reader
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         use Ada.Directories;
      begin
         Result.Add (Read_Config (Full_Name (Directory_Entry)));
      end Call_Reader;

      -------------
      -- Recurse --
      -------------

      procedure Recurse
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         use Ada.Directories;
         Name : constant String := Simple_Name (Directory_Entry);
      begin
         if Name = "." or else Name = ".." then
            return;
         end if;

         Ada.Directories.Search
           (Directory      => Full_Name (Directory_Entry),
            Pattern        => "*." & Extension,
            Filter         => (Ada.Directories.Ordinary_File => True,
                               others                        => False),
            Process        => Call_Reader'Access);

         Ada.Directories.Search
           (Directory      => Full_Name (Directory_Entry),
            Pattern        => "*",
            Filter         => (Ada.Directories.Directory     => True,
                               others                        => False),
            Process        => Recurse'Access);
      end Recurse;

   begin
      Ada.Directories.Search
        (Directory      => Path,
         Pattern        => "*." & Extension,
         Filter         => (Ada.Directories.Ordinary_File => True,
                            others => False),
         Process        => Call_Reader'Access);

      Ada.Directories.Search
        (Directory      => Path,
         Pattern        => "*",
         Filter         => (Ada.Directories.Directory     => True,
                            others => False),
         Process        => Recurse'Access);
      return Result;

   end Read_Config;

   -----------------
   -- Read_Config --
   -----------------

   procedure Read_Config (Path : String;
                          Extension : String;
                          Configure : not null access procedure
                          (Config : Configuration))
   is
      procedure Call_Reader
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      -----------------
      -- Call_Reader --
      -----------------

      procedure Call_Reader
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         use Ada.Directories;
         Config : constant Configuration :=
                    Read_Config (Full_Name (Directory_Entry));
      begin
         Configure (Config);
      exception
         when others =>
            Ada.Text_IO.Put_Line
              ("Error while configuring " & Config.Config_Name);
            raise;
      end Call_Reader;

   begin
      Ada.Directories.Search
        (Directory      => Path,
         Pattern        => "*." & Extension,
         Filter         => (Ada.Directories.Ordinary_File => True,
                            Ada.Directories.Directory     => True,
                            others => False),
         Process        => Call_Reader'Access);
   end Read_Config;

   ---------------------
   -- Read_CSV_Config --
   ---------------------

   function Read_CSV_Config (Path      : String;
                             Separator : Character := ',')
                             return Configuration
   is
      use Ada.Strings.Unbounded;

      Result : Configuration :=
        New_Config (Ada.Directories.Base_Name
                    (Ada.Directories.Simple_Name (Path)));
      File   : Ada.Text_IO.File_Type;

      type Line_Info is
        array (Positive range <>) of Unbounded_String;

      function Next_Line return Line_Info;

      ---------------
      -- Next_Line --
      ---------------

      function Next_Line return Line_Info is
         Line : constant String := Ada.Text_IO.Get_Line (File);
         Result : Line_Info (1 .. Line'Last);
         Start  : Positive := 1;
         Count  : Natural := 0;
      begin
         for I in Line'Range loop
            if Line (I) = Separator then
               Count := Count + 1;
               Result (Count) := To_Unbounded_String (Line (Start .. I - 1));
               Start := I + 1;
            end if;
         end loop;
         return Result (1 .. Count);
      end Next_Line;

   begin

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);

      declare
         Headings : constant Line_Info := Next_Line;
      begin
         while not Ada.Text_IO.End_Of_File (File) loop
            declare
               Current : constant Line_Info := Next_Line;
               Line_Config : Configuration;
            begin
               if Current'Length = Headings'Length then
                  Line_Config := New_Config ("item");

                  for I in Headings'Range loop
                     Line_Config.Add (To_String (Headings (I)),
                                      To_String (Current (I)));
                  end loop;
                  Result.Add (Line_Config);
               end if;
            end;
         end loop;
      end;

      Ada.Text_IO.Close (File);

      return Result;

   end Read_CSV_Config;

   --------------------------
   -- Read_Indirect_Config --
   --------------------------

   function Read_Indirect_Config (Path : String)
                                  return Configuration
   is
      Result : constant Configuration :=
                 Read_Config (Path);
   begin

      for I in 1 .. Result.Children.Last_Index loop
         declare
            Old_Child : constant Configuration_Access :=
                          Result.Children.Element (I);
            Old_Child_Tag : constant String := Old_Child.Config_Name;
            Relative_Path   : constant String :=
                              Result.Children.Element (I).Value;
            Full_Path     : constant String :=
                              Ada.Directories.Containing_Directory (Path) &
            "/" & Relative_Path;
         begin
            Old_Child.all :=
              Read_Config (Full_Path);
            Old_Child.Add ("tag", Old_Child_Tag);
         end;
      end loop;
      return Result;
   end Read_Indirect_Config;

end Tropos.Reader;
