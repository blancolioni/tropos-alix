package body Tropos is

   ---------
   -- Add --
   ---------

   procedure Add
     (To_Config : in out Configuration;
      Child     : in     Configuration)
   is
   begin
      To_Config.Children.Append
        (new Configuration'(Child));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (To_Config : in out Configuration;
      Name      : in     String;
      Value     : in     String)
   is
      Child_Name  : Configuration := New_Config (Name);
      Child_Value : constant Configuration :=
        New_Config (Value);
   begin
      Child_Name.Add (Child_Value);
      To_Config.Add (Child_Name);
   end Add;

   -----------
   -- Child --
   -----------

   function Child (Of_Config  : Configuration;
                   Child_Name : String)
                  return Configuration
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I in 1 .. Of_Config.Children.Last_Index loop
         declare
            Child_Config : constant Configuration_Access :=
                             Of_Config.Children.Element (I);
         begin
            if Child_Config.Name = Child_Name then
               return Of_Config.Children.Element (I).all;
            end if;
         end;
      end loop;
      raise Constraint_Error with
        "configuration " &
        Ada.Strings.Unbounded.To_String (Of_Config.Name) &
        " has no child named " & Child_Name;
   end Child;

   -----------
   -- Child --
   -----------

   function Child (Of_Config  : Configuration;
                   Index      : Positive)
                  return Configuration
   is
   begin
      return Of_Config.Children.Element (Index).all;
   end Child;

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count (Config : Configuration) return Natural is
   begin
      if Config.Children /= null then
         return Config.Children.Last_Index;
      else
         return 0;
      end if;
   end Child_Count;

   --------------
   -- Children --
   --------------

   function Children (Config : Configuration;
                      Name   : String)
                      return Configuration_Array
   is
      Result : Configuration_Array (1 .. Config.Child_Count);
      Count  : Natural := 0;
   begin
      for I in Result'Range loop
         if Config.Children.Element (I).Config_Name = Name then
            Count := Count + 1;
            Result (Count) := Config.Children.Element (I).all;
         end if;
      end loop;
      return Result (1 .. Count);
   end Children;

   -----------------
   -- Config_Name --
   -----------------

   function Config_Name
     (Item : Configuration)
     return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Config_Name;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Config   : in     Configuration;
      Name     : in     String;
      Field    :    out Field_Type)
   is
      Child : constant Configuration'Class := Config.Child (Name);
      Value : constant String := Child.Children.Element (1).Config_Name;
   begin
      Field := From_String (Value);
   end Configure;

   -------------------------
   -- Configure_Container --
   -------------------------

   procedure Configure_Container
     (Config    : in     Configuration;
      Add       : not null access procedure (Config : Configuration))
   is
      It    : Cursor := Config.First;
   begin
      while Has_Element (It) loop
         Add (Element (It));
         Next (It);
      end loop;
--        for Item of Config loop
--           Add (Item);
--        end loop;
   end Configure_Container;

   -------------------------
   -- Configure_Structure --
   -------------------------

   procedure Configure_Structure
     (Config    : in     Configuration;
      Name      : in     String;
      Structure :    out Structure_Type)
   is
      Child : constant Configuration := Config.Child (Name);
   begin
      Configure (Structure, Child);
   end Configure_Structure;

   --------------
   -- Contains --
   --------------

   function Contains (Config  : Configuration;
                      Name    : String)
                      return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for I in 1 .. Config.Children.Last_Index loop
         if Config.Children.Element (I).Name = Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   -------------
   -- Element --
   -------------

   function Element (Item : Cursor) return Configuration is
   begin
      return Element (Item).all;
   end Element;

   -----------
   -- First --
   -----------

   function First (Item : Configuration) return Cursor is
   begin
      if Item.Children = null then
         return No_Element;
      else
         return Cursor (Item.Children.First);
      end if;
   end First;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return String
   is
   begin
      return From_Config.Child (Field_Name).Children.Element (1).Config_Name;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : String)
                 return String
   is
   begin
      if From_Config.Contains (Field_Name) then
         return From_Config.Get (Field_Name);
      else
         return Default_Value;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return String
   is
   begin
      return From_Config.Children.Element (Field_Index).Config_Name;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return Boolean
   is
      Result : constant String := From_Config.Get (Field_Name, "no");
   begin
      return Result = "yes";
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Boolean)
                 return Boolean
   is
      Result : constant String := From_Config.Get (Field_Name, "");
   begin
      if Result = "" then
         return Default_Value;
      else
         return Result = "yes";
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return Integer
   is
      Result : constant String := From_Config.Get (Field_Name, "0");
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            return Integer'Value (Result (1 .. I - 1));
         end if;
      end loop;
      return Integer'Value (Result);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Integer)
                 return Integer
   is
   begin
      if From_Config.Contains (Field_Name) then
         return From_Config.Get (Field_Name);
      else
         return Default_Value;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Integer
   is
      Result : constant String := From_Config.Get (Field_Index);
   begin
      for I in Result'Range loop
         if Result (I) = '.' then
            return Integer'Value (Result (1 .. I - 1));
         end if;
      end loop;
      return Integer'Value (Result);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return Float
   is
      Result : constant String := From_Config.Get (Field_Name, "0.0");
   begin
      return Float'Value (Result);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Float)
                 return Float
   is
      Result : constant String :=
                 From_Config.Get (Field_Name,
                                  Float'Image (Default_Value));
   begin
      return Float'Value (Result);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return Long_Float
   is
      Result : constant String := From_Config.Get (Field_Name, "0.0");
   begin
      return Long_Float'Value (Result);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Long_Float)
                 return Long_Float
   is
      Result : constant String :=
                 From_Config.Get (Field_Name,
                                  Long_Float'Image (Default_Value));
   begin
      return Long_Float'Value (Result);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Float
   is
      Result : constant String := From_Config.Get (Field_Index);
   begin
      return Float'Value (Result);
   end Get;

   --------------
   -- Get_Enum --
   --------------

   function Get_Enum
     (Config   : in     Configuration;
      Name     : in     String)
      return Enum
   is
      Value : constant String := Config.Get (Name);
   begin
      return Enum'Value (Value);
   end Get_Enum;

   ---------------------------
   -- Get_Enum_With_Default --
   ---------------------------

   function Get_Enum_With_Default
     (Config   : in     Configuration;
      Name     : in     String)
      return Enum
   is
      Value : constant String := Config.Get (Name, "");
   begin
      if Value /= "" then
         return Enum'Value (Value);
      else
         return Default;
      end if;
   end Get_Enum_With_Default;
   -----------------
   -- Has_Element --
   -----------------

   overriding
   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Configuration_Vector.Has_Element
        (Configuration_Vector.Cursor (Position));
   end Has_Element;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Config   : Configuration;
      Child_Name : String;
      Process    : not null access procedure (Position : Cursor))
   is
      It : Cursor := Config.First;
   begin
      while Has_Element (It) loop
         if Element (It).Config_Name = Child_Name then
            Process (It);
         end if;
         Next (It);
      end loop;
   end Iterate;

   ----------------
   -- New_Config --
   ----------------

   function New_Config (Name : String) return Configuration is
   begin
      return (Ada.Strings.Unbounded.To_Unbounded_String (Name),
              new Configuration_Vector.Vector);
   end New_Config;

   ----------
   -- Next --
   ----------

   overriding
   procedure Next (Item : in out Cursor) is
   begin
      Configuration_Vector.Next (Configuration_Vector.Cursor (Item));
   end Next;

   -----------
   -- Value --
   -----------

   function Value (Of_Config : Configuration)
                   return String
   is
   begin
      return Of_Config.Children.Element (1).Config_Name;
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Of_Config : Configuration)
                   return Integer
   is
      Result : constant String := Value (Of_Config);
   begin
      return Integer'Value (Result);
   end Value;

   -----------
   -- Value --
   -----------

   function Value (Of_Config : Configuration)
                   return Float
   is
      Result : constant String := Value (Of_Config);
   begin
      return Float'Value (Result);
   end Value;

end Tropos;
