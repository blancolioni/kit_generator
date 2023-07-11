with Syn.Declarations;

with Kit.Schema.Databases;
with Kit.Schema.Tables;

package Kit.Generate.Get_From_Cache is

   function Generate_Get_From_Cache
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : Syn.Declarations.Package_Type'Class)
     return Syn.Declarations.Package_Type'Class;

end Kit.Generate.Get_From_Cache;
