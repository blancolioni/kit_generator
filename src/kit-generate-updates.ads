with Syn.Declarations;
with Kit.Schema.Tables;

package Kit.Generate.Updates is

   procedure Create_Update_Type
     (Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class);

   procedure Generate_Update_Subprograms
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class);

end Kit.Generate.Updates;
