with Syn.Declarations;

with Kit.Schema.Tables;

package Kit.Generate.Database_Package is

   function Generate_Database_Package
     (Db : Kit.Schema.Databases.Database_Type)
      return Syn.Declarations.Package_Type;

   function Generate_Table_Database_Package
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type)
      return Syn.Declarations.Package_Type;

   function Generate_Database_Types_Package
     (Db    : Kit.Schema.Databases.Database_Type)
      return Syn.Declarations.Package_Type;

end Kit.Generate.Database_Package;
