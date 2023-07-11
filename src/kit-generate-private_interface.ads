with Syn.Declarations;
with Kit.Schema.Databases;
with Kit.Schema.Tables;

package Kit.Generate.Private_Interface is

   function Generate_Private_Interface
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : Syn.Declarations.Package_Type'Class)
     return Syn.Declarations.Package_Type'Class;

end Kit.Generate.Private_Interface;
