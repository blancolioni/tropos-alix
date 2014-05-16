with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Tropos.Reader.Parser;

package body Tropos.Reader is

   procedure Parse_Config
     (Config : out Configuration);

   ------------------
   -- Parse_Config --
   ------------------

   procedure Parse_Config
     (Config : out Configuration)
   is
      use Tropos.Reader.Parser;

      Have_Body : Boolean := Tok = Tok_Open_Brace;
   begin
      if Tok = Tok_Name or else Tok = Tok_Open_Brace then
         if Tok = Tok_Name then
            Config.Name :=
              Ada.Strings.Unbounded.To_Unbounded_String (Tok_Text);
            Next;
            if Tok = Tok_Equal then
               Next;
               Have_Body := True;
            end if;
         else
            Config.Name := Ada.Strings.Unbounded.Null_Unbounded_String;
         end if;

         if Have_Body then
            if Tok = Tok_Name then
               Config.Add (New_Config (Tok_Text));
               Next;
            elsif Tok = Tok_Open_Brace then
               Next;
               while not End_Of_File and then
                  Tok /= Tok_Close_Brace
               loop
                  declare
                     Child : Configuration;
                  begin
                     Parse_Config (Child);
                     Config.Add (Child);
                  end;
               end loop;
               if End_Of_File then
                  null;
                  --  Error ("missing '}' at end of file");
               else
                  Next;
               end if;
            else
               Error ("expected name or '{'");
            end if;
         end if;
      else
         Error ("missing name");
      end if;

   end Parse_Config;

   -----------------
   -- Read_Config --
   -----------------

   function Read_Config (Path : String) return Configuration is
      use Tropos.Reader.Parser;
      Result : Configuration :=
        New_Config (Ada.Directories.Base_Name
                    (Ada.Directories.Simple_Name (Path)));
   begin
      Tropos.Reader.Parser.Open (Path);
      while not Tropos.Reader.Parser.End_Of_File loop
         exit when Tok = Tok_Close_Brace;
         declare
            Acc   : constant Configuration_Access := new Configuration;
         begin
            Parse_Config (Acc.all);
            Result.Children.Append (Acc);
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
      Result : aliased Configuration :=
        New_Config (Ada.Directories.Base_Name
                    (Ada.Directories.Simple_Name (Path)));
      Current : Configuration_Access := null;

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
         if Current = null then
            Result.Add (Read_Config (Full_Name (Directory_Entry)));
         else
            Current.Add (Read_Config (Full_Name (Directory_Entry)));
         end if;
      end Call_Reader;

      -------------
      -- Recurse --
      -------------

      procedure Recurse
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         use Ada.Directories;
         Name : constant String := Simple_Name (Directory_Entry);
         Local_Config : constant Configuration_Access := new Configuration;
         Previous_Current : constant Configuration_Access := Current;
      begin
         if Name = "." or else Name = ".." then
            return;
         end if;

         Local_Config.Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Simple_Name (Directory_Entry));
         Current := Local_Config;
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
         Current := Previous_Current;
         if Current = null then
            Result.Children.Append (Local_Config);
         else
            Current.Children.Append (Local_Config);
         end if;
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

   function Read_CSV_Config (Path        : String;
                             Header_Line : Boolean := True;
                             Separator   : Character := ',')
                             return Configuration
   is
      use Ada.Strings.Unbounded;

      Result : Configuration :=
        New_Config (Ada.Directories.Base_Name
                    (Ada.Directories.Simple_Name (Path)));
      File   : Ada.Text_IO.File_Type;

      type Line_Info is
        array (Positive range <>) of Unbounded_String;

      function Read_Header return Line_Info;

      function Next_Line
        (Header : Boolean := False)
         return Line_Info;

      ---------------
      -- Next_Line --
      ---------------

      function Next_Line
        (Header : Boolean := False)
         return Line_Info
      is
         Raw_Line : constant String :=
                      Ada.Text_IO.Get_Line (File);
         Line     : constant String :=
                      (if Raw_Line (Raw_Line'Last) < ' '
                       then Raw_Line (Raw_Line'First .. Raw_Line'Last - 1)
                       else Raw_Line);
         Result : Line_Info (1 .. Line'Last);
         Start  : Positive := 1;
         Finish : Natural := 1;
         Count  : Natural := 0;
      begin
         for I in Line'Range loop
            if Line (I) = Separator
              or else I = Line'Last
            then
               Finish := (if Line (I) = Separator
                          then I - 1
                          else I);
               if not Header or else Finish >= Start then
                  Count := Count + 1;
                  Result (Count) :=
                    To_Unbounded_String (Line (Start .. Finish));
                  Start := I + 1;
               end if;
            end if;
         end loop;
         while Count < 10 loop
            Count := Count + 1;
            Result (Count) :=
              To_Unbounded_String
                (Ada.Strings.Fixed.Trim
                     (Natural'Image (Count),
                      Ada.Strings.Left));
            Count := Count + 1;
         end loop;

         return Result (1 .. Count);
      end Next_Line;

      -----------------
      -- Read_Header --
      -----------------

      function Read_Header return Line_Info is
      begin
         if Header_Line then
            return Next_Line (Header => True);
         else
            declare
               Result : Line_Info (1 .. 10);
            begin
               for I in Result'Range loop
                  Result (I) :=
                    To_Unbounded_String
                      (Ada.Strings.Fixed.Trim
                           (Natural'Image (I),
                            Ada.Strings.Left));
               end loop;
               return Result;
            end;
         end if;
      end Read_Header;

   begin

      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Path);

      declare
         Headings : constant Line_Info := Read_Header;
      begin
         while not Ada.Text_IO.End_Of_File (File) loop
            declare
               Current : constant Line_Info := Next_Line;
               Line_Config : Configuration;
            begin
               Line_Config := New_Config ("item");

               for I in Current'Range loop
                  exit when I not in Headings'Range;
                  Line_Config.Add (To_String (Headings (I)),
                                   To_String (Current (I)));
               end loop;
               Result.Add (Line_Config);
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
         if Result.Children.Element (I).Value = "yes"
           or else Result.Children.Element (I).Value = "no"
         then
            --  leave this one alone
            null;
         else
            declare
               Old_Child       : constant Configuration_Access :=
                                   Result.Children.Element (I);
               Old_Child_Tag   : constant String := Old_Child.Config_Name;
               Relative_Path   : constant String :=
                                   Result.Children.Element (I).Value;
               Full_Path       : constant String :=
                                   Ada.Directories.Containing_Directory
                                     (Path) &
                                   "/" & Relative_Path;
            begin
               Old_Child.all :=
                 Read_Config (Full_Path);
               Old_Child.Add ("tag", Old_Child_Tag);
            end;
         end if;
      end loop;
      return Result;
   end Read_Indirect_Config;

end Tropos.Reader;
