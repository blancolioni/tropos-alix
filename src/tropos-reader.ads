package Tropos.Reader is

   function Read_Config (Path : String) return Configuration;
   function Read_Config (Path      : String;
                         Extension : String)
                         return Configuration;

   procedure Read_Config (Path : String;
                          Extension : String;
                          Configure : not null access procedure
                          (Config : Configuration));

   procedure Read_Config (Path : String;
                          Configure : not null access procedure
                          (Config : Configuration));

   function Read_Indirect_Config (Path : String)
                                  return Configuration;

   function Read_CSV_Config (Path      : String;
                             Separator : Character := ',')
                             return Configuration;

end Tropos.Reader;
