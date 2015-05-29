with Ada.Strings.Fixed;

package body Tropos is

   type Iterator is
     new Configuration_Iterator_Interfaces.Reversible_Iterator
   with record
      Container : Configuration_Access;
      Current   : Cursor;
   end record;

   overriding function First (Object : Iterator) return Cursor;
   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

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

   ---------
   -- Add --
   ---------

   procedure Add (To_Config : in out Configuration;
                  Name      : in     String;
                  Value     : in     Integer)
   is
   begin
      To_Config.Add (Name,
                     Ada.Strings.Fixed.Trim
                       (Integer'Image (Value),
                        Ada.Strings.Left));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (To_Config : in out Configuration;
                  Name      : in     String;
                  Value     : in     Float)
   is
   begin
      To_Config.Add (Name,
                     Ada.Strings.Fixed.Trim
                       (Float'Image (Value),
                        Ada.Strings.Left));
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
      return Config.Children.Last_Index;
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

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Configuration;
      Position  : Cursor)
      return Constant_Reference_Type
   is
      pragma Unreferenced (Container);
      Item : constant Configuration_Access :=
               Configuration_Vector.Element (Position.Position);
   begin
      return (Element => Item);
   end Constant_Reference;

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
      return Configuration_Vector.Element (Item.Position).all;
   end Element;

   -----------
   -- First --
   -----------

   function First (Item : Configuration) return Cursor is
   begin
      if Item.Children.Is_Empty then
         return No_Element;
      else
         return (Position => Item.Children.First);
      end if;
   end First;

   -----------
   -- First --
   -----------

   overriding function First (Object : Iterator) return Cursor is
   begin
      if Object.Current = No_Element then
         return Object.Container.First;
      else
         return Object.Current;
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
      if From_Config.Contains (Field_Name)
        and then From_Config.Get (Field_Name) /= ""
      then
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
      Result : constant String :=
                 Ada.Strings.Fixed.Trim (From_Config.Get (Field_Name, "0.0"),
                                         Ada.Strings.Both);
   begin
      for I in Result'Range loop
         if Result (I) not in '0' .. '9' and then Result (I) /= '.'
           and then Result (I) /= 'e' and then Result (I) /= 'E'
           and then Result (I) /= '+' and then Result (I) /= '-'
         then
            return Float'Value (Result (Result'First .. I - 1));
         end if;
      end loop;
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

   ---------
   -- Get --
   ---------

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Long_Float
   is
      Result : constant String := From_Config.Get (Field_Index);
   begin
      return Long_Float'Value (Result);
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

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Configuration_Vector.Has_Element (Position.Position);
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

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Configuration)
      return Configuration_Iterator_Interfaces.Reversible_Iterator'Class
   is

      V : constant Configuration_Access := Container'Unrestricted_Access;
      Result : Iterator :=
                 (Container => V, Current => No_Element);
   begin
      return Result;
   end Iterate;

   ----------
   -- Last --
   ----------

   overriding function Last (Object : Iterator) return Cursor is
   begin
      if Object.Current = No_Element then
         return (Position => Object.Container.Children.Last);
      else
         return Object.Current;
      end if;
   end Last;

   ----------------
   -- New_Config --
   ----------------

   function New_Config (Name : String) return Configuration is
   begin
      return (Ada.Strings.Unbounded.To_Unbounded_String (Name),
              Configuration_Vector.Empty_Vector);
   end New_Config;

   ----------------
   -- New_Config --
   ----------------

   function New_Config (Index : Integer) return Configuration is
      use Ada.Strings.Unbounded;
   begin
      return (Trim
              (To_Unbounded_String
                 (Integer'Image (Index)),
                 Ada.Strings.Left),
              Configuration_Vector.Empty_Vector);
   end New_Config;

   ----------
   -- Next --
   ----------

   procedure Next (Item : in out Cursor) is
   begin
      Configuration_Vector.Next (Item.Position);
   end Next;

   ----------
   -- Next --
   ----------

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return (Position => Configuration_Vector.Next (Position.Position));
   end Next;

   --------------
   -- Previous --
   --------------

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
      pragma Unreferenced (Object);
   begin
      return (Position => Configuration_Vector.Previous (Position.Position));
   end Previous;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Container : aliased in out Configuration;
      Position  : Cursor)
      return Reference_Type
   is
      pragma Unreferenced (Container);
      Item : constant Configuration_Access :=
               Configuration_Vector.Element (Position.Position);
   begin
      return (Element => Item);
   end Reference;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path
     (Config         : in out Configuration;
      Path           : String;
      Value          : String;
      Path_Separator : Character := '.')
   is
      use Ada.Strings, Ada.Strings.Fixed;
      Sep_Index : constant Natural :=
                    Index (Path, (1 => Path_Separator));
   begin
      if Sep_Index = 0 then
         Config.Add (Path, Value);
      else
         declare
            Field : constant String := Path (Path'First .. Sep_Index - 1);
         begin
            if Config.Contains (Field) then
               declare
                  use Ada.Strings.Unbounded;
               begin
                  for Child of Config.Children loop
                     if Child.Name = Field then
                        Child.Set_Path
                          (Path (Sep_Index + 1 .. Path'Last),
                           Value, Path_Separator);
                        exit;
                     end if;
                  end loop;
               end;
            else
               declare
                  Child_Config : Configuration :=
                                   New_Config (Field);
               begin
                  Child_Config.Set_Path
                    (Path (Sep_Index + 1 .. Path'Last),
                     Value, Path_Separator);
                  Config.Add (Child_Config);
               end;
            end if;
         end;
      end if;
   end Set_Path;

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
