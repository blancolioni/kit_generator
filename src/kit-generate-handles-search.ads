with Kit.Schema.Keys;

package Kit.Generate.Handles.Search is

   procedure Create_Selection_Type
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Selection_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String;
      Key_Value     : Boolean;
      Bounds        : Boolean;
      Bounded_Index : Natural   := 0);

   procedure Create_Unique_Get_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String);

   procedure Create_First_Last_Functions
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String);

   procedure Create_Default_Key_Functions
     (Db         : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key           : Kit.Schema.Keys.Key_Type);

end Kit.Generate.Handles.Search;
