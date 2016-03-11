with Aqua.IO;
with Aqua.Objects;
with Aqua.Primitives;

with Tropos.Reader;

package body Tropos.Aqua_Objects is

   function Handle_Read_Config
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
     with Pre => Arguments'Length = 1
     and then Aqua.Is_String_Reference (Arguments (Arguments'First));

   ------------------------
   -- Handle_Read_Config --
   ------------------------

   function Handle_Read_Config
     (Context   : in out Aqua.Execution.Execution_Interface'Class;
      Arguments : Aqua.Array_Of_Words)
      return Aqua.Word
   is
      Path : constant String :=
               Context.To_String (Arguments (Arguments'First));
      Config : constant Configuration := Tropos.Reader.Read_Config (Path);
   begin
      return To_Aqua_Object (Context, Config);
   end Handle_Read_Config;

   ---------------------
   -- Register_Tropos --
   ---------------------

   procedure Register_Tropos is
      use Aqua.Primitives;
   begin
      New_Primitive_Function ("tropos_read_config", 1,
                              Handle_Read_Config'Access);
   end Register_Tropos;

   --------------------
   -- To_Aqua_Object --
   --------------------

   function To_Aqua_Object
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Config  : Tropos.Configuration)
      return Aqua.Word
   is
   begin
      if Config.Child_Count = 0 then
         return Context.To_String_Word (Config.Config_Name);
      else
         declare
            Result : constant Aqua.Objects.Object_Access :=
                       new Aqua.Objects.Root_Object_Type;
         begin
            for Child of Config loop
               Result.Set_Property
                 (Child.Config_Name,
                  To_Aqua_Object (Context, Child));
            end loop;
            return Context.To_Word (Result);
         end;
      end if;
   end To_Aqua_Object;

   ----------------------
   -- To_Configuration --
   ----------------------

   function To_Configuration
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Object  : Aqua.Word)
      return Configuration
   is
      use Aqua;

      Result : Configuration;

      procedure Add_Config
        (Property_Name  : String;
         Property_Value : Word);

      ----------------
      -- Add_Config --
      ----------------

      procedure Add_Config
        (Property_Name  : String;
         Property_Value : Word)
      is
         Child : Configuration :=
                   To_Configuration (Context, Property_Value);
      begin
         Child.Name :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (Property_Name);
         Result.Add (Child);
      end Add_Config;

   begin
      if Is_Integer (Object) then
         Result.Add (New_Config (Integer (Get_Integer (Object))));
      elsif Is_String_Reference (Object) then
         Result.Add (New_Config (Context.To_String (Object)));
      elsif not Is_External_Reference (Object) then
         Result.Add (New_Config (Aqua.IO.Hex_Image (Object)));
      else
         declare
            Ext : constant access External_Object_Interface'Class :=
              Context.To_External_Object (Object);
         begin
            if Ext.all not in Aqua.Objects.Object_Interface'Class then
               return New_Config (Ext.Name);
            else
               Aqua.Objects.Object_Interface'Class (Ext.all).Scan_Properties
                 (Add_Config'Access);
            end if;
         end;
      end if;
      return Result;
   end To_Configuration;

end Tropos.Aqua_Objects;
