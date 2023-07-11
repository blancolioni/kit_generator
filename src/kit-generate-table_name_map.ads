with Syn.Declarations;

package Kit.Generate.Table_Name_Map is

   function Generate_Package
     (Db : Kit.Schema.Databases.Database_Type)
      return Syn.Declarations.Package_Type;

end Kit.Generate.Table_Name_Map;
