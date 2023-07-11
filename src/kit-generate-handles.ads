with Syn.Declarations;
with Kit.Schema.Tables;

package Kit.Generate.Handles is

   function Generate_Handle_Package
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : Syn.Declarations.Package_Type'Class)
      return Syn.Declarations.Package_Type'Class;

end Kit.Generate.Handles;
