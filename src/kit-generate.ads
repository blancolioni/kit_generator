with Syn.Projects;
with Kit.Schema.Databases;

package Kit.Generate is

   type Generated_Database_Type is
     (Btree_Marlowe,
      Memory_Marlowe);

   function Generated_Database return Generated_Database_Type;

   function Data_Store_Package_Name return String;
   function Data_Store_Type_Name return String;
   function Data_Store_Cursor_Name return String;

   function Generate_Database
     (Db       : Kit.Schema.Databases.Database_Type)
      return Syn.Projects.Project;

   procedure Set_Options
     (Ada_2022           : Boolean := False;
      Deadlock_Detection : Boolean := False;
      Debug              : Boolean := False;
      Output_Directory   : String  := ".");

private

   function Create_Aggregate (Content : String) return String;

   function Option_Ada_2022 return Boolean;
   function Option_Debug return Boolean;
   function Option_Deadlock_Detection return Boolean;
   function Option_Output_Directory return String;

end Kit.Generate;
