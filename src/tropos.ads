private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Ada.Iterator_Interfaces;

package Tropos is

   type Configuration is tagged private
   with
      Constant_Indexing => Constant_Reference,
      Variable_Indexing => Reference,
      Default_Iterator  => Iterate,
      Iterator_Element  => Configuration;

   Empty_Config : constant Configuration;

   function New_Config (Name : String) return Configuration;
   function New_Config (Index : Integer) return Configuration;

   procedure Add (To_Config : in out Configuration;
                  Child     : in     Configuration);

   procedure Add (To_Config : in out Configuration;
                  Name      : in     String;
                  Value     : in     String);

   function Get (From_Config : Configuration;
                 Field_Name  : String)
                 return String;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : String)
                 return String;

   function Get (From_Config   : Configuration;
                 Field_Name    : String)
                 return Integer;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Integer)
                 return Integer;

   function Get (From_Config   : Configuration;
                 Field_Name    : String)
                 return Float;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Float)
                 return Float;

   function Get (From_Config   : Configuration;
                 Field_Name    : String)
                 return Long_Float;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Long_Float)
                 return Long_Float;

   function Get (From_Config   : Configuration;
                 Field_Name    : String)
                 return Boolean;

   function Get (From_Config   : Configuration;
                 Field_Name    : String;
                 Default_Value : Boolean)
                 return Boolean;

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return String;

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Integer;

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Float;

   function Get (From_Config : Configuration;
                 Field_Index : Positive)
                 return Long_Float;

   function Value (Of_Config : Configuration)
                   return String;

   function Value (Of_Config : Configuration)
                   return Integer;

   function Value (Of_Config : Configuration)
                   return Float;

   generic
      type Field_Type is private;
      with function From_String (Text : String) return Field_Type;
   procedure Configure
     (Config   : in     Configuration;
      Name     : in     String;
      Field    :    out Field_Type);

   generic
      type Structure_Type is private;
      with procedure Configure (Item    : in out Structure_Type;
                                Config  : in     Configuration)
         is <>;
   procedure Configure_Structure
     (Config    : in     Configuration;
      Name      : in     String;
      Structure :    out Structure_Type);

   procedure Configure_Container
     (Config    : in     Configuration;
      Add       : not null access procedure (Config : Configuration));

   generic
      type Enum is (<>);
   function Get_Enum
     (Config   : in     Configuration;
      Name     : in     String)
      return Enum;

   generic
      type Enum is (<>);
      Default : Enum;
   function Get_Enum_With_Default
     (Config   : in     Configuration;
      Name     : in     String)
      return Enum;

   function Config_Name
     (Item : Configuration)
     return String;

   type Cursor is private;
   No_Element : constant Cursor;

   function First (Item : Configuration) return Cursor;
   function Element (Item : Cursor) return Configuration;
   procedure Next (Item : in out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   package Configuration_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Constant_Reference_Type
      (Element : not null access constant Configuration) is
   private
   with
      Implicit_Dereference => Element;

   type Reference_Type (Element : not null access Configuration) is private
   with
      Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Configuration;
      Position  : Cursor) return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   function Reference
     (Container : aliased in out Configuration;
      Position  : Cursor) return Reference_Type;
   pragma Inline (Reference);

   function Child (Of_Config  : Configuration;
                   Child_Name : String)
                   return Configuration;

   function Child (Of_Config  : Configuration;
                   Index      : Positive)
                   return Configuration;

   function Contains (Config : Configuration;
                      Name   : String)
                      return Boolean;

   function Child_Count (Config : Configuration) return Natural;

   procedure Iterate
     (Config   : Configuration;
      Child_Name : String;
      Process    : not null access procedure (Position : Cursor));

   function Iterate
     (Container : Configuration)
      return Configuration_Iterator_Interfaces.Reversible_Iterator'Class;

   type Configuration_Array is array (Positive range <>) of Configuration;

   function Children (Config : Configuration;
                      Name   : String)
                      return Configuration_Array;

private

   type Configuration_Access is access all Configuration;

   package Configuration_Vector is
      new Ada.Containers.Vectors (Positive, Configuration_Access);

   type Configuration is tagged
      record
         Name     : Ada.Strings.Unbounded.Unbounded_String;
         Children : Configuration_Vector.Vector;
      end record;

   type Constant_Reference_Type
     (Element : not null access constant Configuration) is null record;

   type Reference_Type
     (Element : not null access Configuration) is null record;

   type Cursor is
      record
         Position : Configuration_Vector.Cursor;
      end record;

   Empty_Config : constant Configuration :=
                    (Ada.Strings.Unbounded.Null_Unbounded_String,
                     Configuration_Vector.Empty_Vector);

   No_Element : constant Cursor :=
                  (Position => Configuration_Vector.No_Element);

end Tropos;
