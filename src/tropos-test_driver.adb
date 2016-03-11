with Tropos.Aqua_Objects;
with Tropos.Paths;
with Tropos.Reader;
with Tropos.Writer;

procedure Tropos.Test_Driver is
   Config : constant Configuration :=
              Tropos.Reader.Read_Config
                (Tropos.Paths.Config_File ("stars.txt"));
begin
   Tropos.Aqua_Objects.Register_Tropos;
   Tropos.Writer.Write_XML_Config
     (Config, Tropos.Paths.Config_File ("stars.xml"));
   Tropos.Writer.Write_Scheme_Config
     (Config, Tropos.Paths.Config_File ("stars.scm"));
end Tropos.Test_Driver;
