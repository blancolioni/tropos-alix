with Aqua.Execution;

package Tropos.Aqua_Objects is

   function To_Aqua_Object
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Config  : Tropos.Configuration)
      return Aqua.Word;

   function To_Configuration
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Object  : Aqua.Word)
      return Configuration;

   procedure Register_Tropos;

end Tropos.Aqua_Objects;
