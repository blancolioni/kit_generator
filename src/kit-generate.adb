with Ada.Strings.Unbounded;

with Syn.Declarations;
with Syn.Expressions;
with Syn.Types;

with Kit.Schema.Keys;
with Kit.Schema.Tables;
with Kit.Schema.Types;
with Kit.Schema.Types.Enumerated;

with Kit.Generate.Database_Package;
with Kit.Generate.Get_From_Cache;
with Kit.Generate.Handles;
with Kit.Generate.Marlowe_Keys_Package;
with Kit.Generate.Public_Interface;
with Kit.Generate.Private_Interface;
with Kit.Generate.Selections;
with Kit.Generate.Table_Name_Map;

package body Kit.Generate is

   Ada_2022_Option           : Boolean := False;
   Deadlock_Detection_Option : Boolean := False;
   Debug_Option              : Boolean := False;
   Output_Directory_Option   : Ada.Strings.Unbounded.Unbounded_String;

   procedure Create_Handle_Function
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type);

   pragma Unreferenced (Create_Handle_Function);

   procedure Create_Table_Type
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type);

   procedure Create_Key_Type
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type);
   pragma Unreferenced (Create_Key_Type);

   procedure Create_Reference_Types
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type);

   procedure Create_User_Defined_Types
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type);

   procedure Create_Record_Interface
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type);

   procedure Create_Search_Interface
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type);

   procedure Create_Locking_Interface
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type);

   ----------------------
   -- Create_Aggregate --
   ----------------------

   function Create_Aggregate (Content : String) return String is
   begin
      if Option_Ada_2022 then
         return '[' & Content & ']';
      else
         return '(' & Content & ')';
      end if;
   end Create_Aggregate;

   ----------------------------
   -- Create_Handle_Function --
   ----------------------------

   procedure Create_Handle_Function
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type)
   is
      pragma Unreferenced (Db);
      use Syn.Declarations;

      Handle : constant Subprogram_Declaration'Class :=
                 New_Function ("Handle",
                               "Kit.Access_Control.Access_Handle",
                               Syn.Object ("Local_Handle"));
      Local_Handle : constant Object_Declaration'Class :=
                       New_Object_Declaration
                         ("Local_Handle",
                          "Kit.Access_Control.Access_Handle");
   begin
      Top.With_Package ("Kit.Access_Control");
      Top.Append_To_Body (Local_Handle);
      Top.Append (Handle);
      Top.Add_Separator;
   end Create_Handle_Function;

   ---------------------
   -- Create_Key_Type --
   ---------------------

   procedure Create_Key_Type
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type)
   is
      procedure Add_Table_Keys
        (Table : Kit.Schema.Tables.Table_Type);

      --------------------
      -- Add_Table_Keys --
      --------------------

      procedure Add_Table_Keys
        (Table : Kit.Schema.Tables.Table_Type)
      is

         Key_Type_Definition : Syn.Enumeration_Type_Definition;

         procedure Add_Key_Type_Literal
           (Base : Kit.Schema.Tables.Table_Type;
            Item : Kit.Schema.Keys.Key_Type);

         --------------------------
         -- Add_Key_Type_Literal --
         --------------------------

         procedure Add_Key_Type_Literal
           (Base : Kit.Schema.Tables.Table_Type;
            Item : Kit.Schema.Keys.Key_Type)
         is
            pragma Unreferenced (Base);
         begin
            Key_Type_Definition.New_Literal
              ("K_" & Table.Name & "_" & Item.Ada_Name);
         end Add_Key_Type_Literal;

      begin
         Table.Scan_Keys (Add_Key_Type_Literal'Access);
         Top.Append
           (Syn.Declarations.New_Full_Type_Declaration
              (Table.Ada_Name & "_Key", Key_Type_Definition));
         Top.Append (Syn.Declarations.New_Separator);
      end Add_Table_Keys;

   begin
      Db.Iterate (Add_Table_Keys'Access);
   end Create_Key_Type;

   ------------------------------
   -- Create_Locking_Interface --
   ------------------------------

   procedure Create_Locking_Interface
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type)
   is
      pragma Unreferenced (Db);
      use Syn.Declarations;

      Lock_Context : Syn.Types.Record_Type_Definition;
      Lock_Record  : Type_Declaration;
      Lock_Access  : Type_Declaration;
      X_Lock       : Subprogram_Declaration'Class :=
                       New_Abstract_Procedure ("X_Lock");
      S_Lock       : Subprogram_Declaration'Class :=
                       New_Abstract_Procedure ("S_Lock");
      Unlock       : Subprogram_Declaration'Class :=
                       New_Abstract_Procedure ("Unlock");
      Memory_Mutex : Object_Declaration'Class :=
                       New_Object_Declaration
                         ("Memory_Mutex", "Kit.Mutex.Mutex_Type");
      Database_Mutex : Object_Declaration'Class :=
                       New_Object_Declaration
                         ("Database_Mutex", "Kit.Mutex.Mutex_Type");

   begin
      Lock_Context.Set_Tagged;
      Lock_Context.Set_Abstract;
      Lock_Context.Add_Component ("S_Locked", "Boolean", "False");
      Lock_Context.Add_Component ("X_Locked", "Boolean", "False");
      Lock_Record :=
        Syn.Declarations.New_Private_Type_Declaration
          ("Lock_Context_Record", Definition => Lock_Context);
      Top.Append (Lock_Record);
      Lock_Access :=
        Syn.Declarations.New_Full_Type_Declaration
          ("Lock_Context",
           Syn.New_Access_Type
             ("Lock_Context_Record'Class",
              Access_All => True));
      --  Lock_Access.Set_Private_Spec;
      Top.Append (Lock_Access);

      --  X_Lock.Set_Private_Spec;
      X_Lock.Add_Formal_Argument ("Context", Inout_Argument,
                                  "Lock_Context_Record");
      Top.Append (X_Lock);

      --  S_Lock.Set_Private_Spec;
      S_Lock.Add_Formal_Argument ("Context", Inout_Argument,
                                  "Lock_Context_Record");
      Top.Append (S_Lock);

      --  Unlock.Set_Private_Spec;
      Unlock.Add_Formal_Argument ("Context", Inout_Argument,
                                  "Lock_Context_Record");
      Top.Append (Unlock);

      Memory_Mutex.Set_Private_Spec;
      Top.Append (Memory_Mutex);
      Database_Mutex.Set_Private_Spec;
      Top.Append (Database_Mutex);

   end Create_Locking_Interface;

   -----------------------------
   -- Create_Record_Interface --
   -----------------------------

   procedure Create_Record_Interface
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type)
   is
      use Syn, Syn.Declarations;
      pragma Unreferenced (Db);
      Record_Interface : Syn.Interface_Type_Definition;
      Update_Interface : Syn.Interface_Type_Definition;
   begin
      Record_Interface.Set_Limited;
      Top.Append (New_Full_Type_Declaration
                  ("Record_Interface", Record_Interface));
      Top.Add_Separator;

      Top.Append
        (New_Abstract_Procedure
           ("X_Lock",
            New_In_Argument
              ("Item", Named_Subtype ("Record_Interface"))));

--        Top.Append
--          (New_Abstract_Function
--             ("Top_Record",
--              New_Formal_Argument
--                ("Item",
--                 Named_Subtype
--                   ("Record_Interface")),
--              Named_Subtype ("Record_Type")));

      Top.Append
        (New_Abstract_Function
           ("Get",
            New_Formal_Argument
              ("Item", Named_Subtype ("Record_Interface")),
            New_Formal_Argument
              ("Field", Named_Subtype ("String")),
            Named_Subtype ("String")));

      Top.Add_Separator;

      Top.Append
        (New_Abstract_Function
           (Name        => "Identity",
            Argument    =>
              New_In_Argument
                ("Item", Named_Subtype ("Record_Interface")),
            Result_Type =>
              Named_Subtype ("String")));

      Top.Add_Separator;

      Update_Interface.Set_Limited;
      Top.Append (New_Full_Type_Declaration
                  ("Record_Update_Interface", Update_Interface));
      Top.Add_Separator;

      Top.Append
        (New_Abstract_Procedure
           ("Set",
            New_Inout_Argument
              ("Item", Named_Subtype ("Record_Update_Interface")),
            New_Formal_Argument
              ("Field", Named_Subtype ("String")),
            New_Formal_Argument
              ("Value", Named_Subtype ("String"))));
      Top.Add_Separator;

   end Create_Record_Interface;

   ----------------------------
   -- Create_Reference_Types --
   ----------------------------

   procedure Create_Reference_Types
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type)
   is

      procedure Add_Reference_Type
        (Item : Kit.Schema.Tables.Table_Type);

      ------------------------
      -- Add_Reference_Type --
      ------------------------

      procedure Add_Reference_Type
        (Item : Kit.Schema.Tables.Table_Type)
      is
         use Syn, Syn.Declarations;
         use Syn.Expressions;
         Reference_Name : constant String :=
                            Item.Ada_Name & "_Reference";
         To_String      : Subprogram_Declaration'Class :=
                            New_Function
                              (Name        => "To_String",
                               Result_Type => "String",
                               Result      =>
                                 New_Function_Call_Expression
                                   (Reference_Name & "'Image",
                                    Object ("Item")));
      begin
         To_String.Add_Formal_Argument
           (Arg_Name => "Item",
            Arg_Type => Reference_Name);

         Top.Append
           (New_Private_Type_Declaration
              (Reference_Name,
               New_Derived_Type ("Marlowe.Database_Index")));

         Top.Append
           (New_Deferred_Constant_Declaration
              ("Null_" & Item.Name & "_Reference",
               Reference_Name,
               Literal (0)));

         Top.Append (To_String);

         declare
            Table_Index : Declaration'Class :=
                            New_Constant_Declaration
                              (Item.Ada_Name & "_Table_Index",
                               Syn.Literal
                                 (Integer (Item.Reference_Index)));
         begin
            Table_Index.Set_Private_Spec;
            Top.Append (Table_Index);
         end;

         Top.Append (New_Separator);
      end Add_Reference_Type;

   begin
      Db.Iterate (Add_Reference_Type'Access);
   end Create_Reference_Types;

   -----------------------------
   -- Create_Search_Interface --
   -----------------------------

   procedure Create_Search_Interface
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type)
   is
      use Syn, Syn.Declarations;
      pragma Unreferenced (Db);
      Search_Interface : Syn.Interface_Type_Definition;
   begin
      Search_Interface.Set_Limited;
      Top.Append (New_Full_Type_Declaration
                  ("Search_Interface", Search_Interface));
      Top.Add_Separator;
      Top.Append
        (New_Abstract_Function
           ("Has_Element",
            New_Formal_Argument
              ("Item",
               Named_Subtype
                 ("Search_Interface")),
            Named_Subtype ("Boolean")));
      Top.Add_Separator;

--        Top.Append
--          (New_Abstract_Procedure
--             ("Next",
--              New_Inout_Argument
--                ("Item", Named_Subtype ("Search_Interface"))));
--        Top.Add_Separator;

   end Create_Search_Interface;

   -----------------------
   -- Create_Table_Type --
   -----------------------

   procedure Create_Table_Type
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type)
   is
      Table_Type_Definition : Syn.Enumeration_Type_Definition;

      procedure Add_Table_Type_Literal
        (Item : Kit.Schema.Tables.Table_Type);

      ----------------------------
      -- Add_Table_Type_Literal --
      ----------------------------

      procedure Add_Table_Type_Literal
        (Item : Kit.Schema.Tables.Table_Type)
      is
      begin
         Table_Type_Definition.New_Literal ("R_" & Item.Name);
      end Add_Table_Type_Literal;

   begin
      Table_Type_Definition.New_Literal ("R_None");
      Db.Iterate (Add_Table_Type_Literal'Access);
      Top.Append
        (Syn.Declarations.New_Full_Type_Declaration
           ("Record_Type", Table_Type_Definition));
      Top.Append (Syn.Declarations.New_Separator);

   end Create_Table_Type;

   -------------------------------
   -- Create_User_Defined_Types --
   -------------------------------

   procedure Create_User_Defined_Types
     (Db  : Kit.Schema.Databases.Database_Type;
      Top : in out Syn.Declarations.Package_Type)
   is

      procedure Create_Type (User_Type : Kit.Schema.Types.Kit_Type);

      -----------------
      -- Create_Type --
      -----------------

      procedure Create_Type (User_Type : Kit.Schema.Types.Kit_Type) is
      begin
         if not User_Type.Is_External_Type then
            if User_Type.all in
              Kit.Schema.Types.Enumerated.Enumerated_Type'Class
            then
               Kit.Schema.Types.Enumerated.Enumerated_Type (User_Type.all)
                 .Set_Defining_Package (Db.Database_Package_Name);
            end if;
            Top.Append (User_Type.To_Declaration);
         end if;
      end Create_Type;

   begin
      Kit.Schema.Types.Iterate_User_Defined_Types (Create_Type'Access);
   end Create_User_Defined_Types;

   ----------------------------
   -- Data_Store_Cursor_Name --
   ----------------------------

   function Data_Store_Cursor_Name return String is
   begin
      return "Marlowe.Data_Stores.Data_Store_Cursor";
   end Data_Store_Cursor_Name;

   -----------------------------
   -- Data_Store_Package_Name --
   -----------------------------

   function Data_Store_Package_Name return String is
   begin
      case Generated_Database is
         when Btree_Marlowe =>
            return "Marlowe.Data_Stores.Btrees";
         when Memory_Marlowe =>
            return "Marlowe.Data_Stores.Memory";
      end case;
   end Data_Store_Package_Name;

   --------------------------
   -- Data_Store_Type_Name --
   --------------------------

   function Data_Store_Type_Name return String is
   begin
      case Generated_Database is
         when Btree_Marlowe =>
            return "Marlowe.Data_Stores.Btrees.Btree_Data_Store";
         when Memory_Marlowe =>
            return "Marlowe.Data_Stores.Memory.Memory_Data_Store";
      end case;
   end Data_Store_Type_Name;

   -----------------------
   -- Generate_Database --
   -----------------------

   function Generate_Database
     (Db : Kit.Schema.Databases.Database_Type)
      return Syn.Projects.Project
   is
      Top_Package : Syn.Declarations.Package_Type :=
        Syn.Declarations.New_Package_Type (Db.Database_Package_Name);
      Handles     : constant Syn.Declarations.Package_Type :=
        Syn.Declarations.New_Package_Type (Db.Handle_Package_Name);

      Project : Syn.Projects.Project;

      function Get_Record_Literal_Name (Index : Positive) return String;

      procedure Get_From_Cache
        (Table : Kit.Schema.Tables.Table_Type);

      procedure Handle_Interface
        (Table : Kit.Schema.Tables.Table_Type);

      procedure Public_Interface
        (Table : Kit.Schema.Tables.Table_Type);

      procedure Private_Interface
        (Table : Kit.Schema.Tables.Table_Type);

      procedure Table_Database_Package
        (Table : Kit.Schema.Tables.Table_Type);

      --------------------
      -- Get_From_Cache --
      --------------------

      procedure Get_From_Cache
        (Table : Kit.Schema.Tables.Table_Type)
      is
      begin
         Project.Add_Package
           (Kit.Generate.Get_From_Cache.Generate_Get_From_Cache
              (Db, Table, Top_Package));
      end Get_From_Cache;

      -----------------------------
      -- Get_Record_Literal_Name --
      -----------------------------

      function Get_Record_Literal_Name (Index : Positive) return String is
      begin
         return "R_" & Db.Element (Index).Ada_Name;
      end Get_Record_Literal_Name;

      ----------------------
      -- Handle_Interface --
      ----------------------

      procedure Handle_Interface
        (Table : Kit.Schema.Tables.Table_Type)
      is
         function Top_Level_Handle_Package
           return Syn.Declarations.Package_Type;

         ------------------------------
         -- Top_Level_Handle_Package --
         ------------------------------

         function Top_Level_Handle_Package
           return Syn.Declarations.Package_Type
         is
            use Syn, Syn.Declarations;
         begin
            return Handle : Package_Type :=
              New_Package_Type (Db.Handle_Package_Name)
            do
               declare
                  Interface_Name : constant String := "Handle_Interface";
                  Interface_Definition : Interface_Type_Definition;
                  Interface_Argument   : constant Formal_Argument'Class :=
                    New_Formal_Argument
                      (Name          => "Handle",
                       Argument_Type => Syn.Named_Subtype (Interface_Name));
                  Operator_Enum        : Syn.Enumeration_Type_Definition;
               begin

                  Handle.Append
                    (New_Full_Type_Declaration
                       (Identifier => Interface_Name,
                        Definition => Interface_Definition));

                  Handle.Append
                    (New_Abstract_Function
                       ("Has_Element",
                        Interface_Argument,
                        Named_Subtype ("Boolean")));

                  Operator_Enum.New_Literal ("Op_None");
                  Operator_Enum.New_Literal ("Op_Not");
                  Operator_Enum.New_Literal ("Op_EQ");
                  Operator_Enum.New_Literal ("Op_NE");
                  Operator_Enum.New_Literal ("Op_LE");
                  Operator_Enum.New_Literal ("Op_GT");
                  Operator_Enum.New_Literal ("Op_LT");
                  Operator_Enum.New_Literal ("Op_GE");

                  declare
                     Operator_Type : Syn.Declarations.Type_Declaration'Class :=
                       Syn.Declarations.New_Full_Type_Declaration
                         ("Constraint_Operator", Operator_Enum);
                  begin
                     Operator_Type.Set_Private_Spec;
                     Handle.Append (Operator_Type);
                  end;

               end;
            end return;
         end Top_Level_Handle_Package;

      begin
         Project.Add_Package (Top_Level_Handle_Package);

         declare
            Table_Handles : constant Syn.Declarations.Package_Type'Class :=
              Kit.Generate.Handles.Generate_Handle_Package
                (Db, Table, Handles);
         begin
            Project.Add_Package (Table_Handles);
            if False then
               Project.Add_Package
                 (Kit.Generate.Selections.Generate_Selection_Package
                    (Db, Table, Table_Handles));
            end if;
         end;
      end Handle_Interface;

      -----------------------
      -- Private_Interface --
      -----------------------

      procedure Private_Interface
        (Table : Kit.Schema.Tables.Table_Type)
      is
      begin
         Project.Add_Package
           (Kit.Generate.Private_Interface.Generate_Private_Interface
              (Db, Table, Top_Package));
      end Private_Interface;

      ----------------------
      -- Public_Interface --
      ----------------------

      procedure Public_Interface
        (Table : Kit.Schema.Tables.Table_Type)
      is
      begin
         Project.Add_Package
           (Kit.Generate.Public_Interface.Generate_Public_Interface
              (Db, Table, Top_Package));
      end Public_Interface;

      ----------------------------
      -- Table_Database_Package --
      ----------------------------

      procedure Table_Database_Package
        (Table : Kit.Schema.Tables.Table_Type)
      is
      begin
         Project.Add_Package
           (Kit.Generate.Database_Package.Generate_Table_Database_Package
              (Db, Table));
      end Table_Database_Package;

   begin

      Kit.Schema.Types.Update_Record_Type (Db.Table_Count,
                                    Get_Record_Literal_Name'Access);

      Top_Package.With_Package ("Marlowe", Private_With => True);
      Top_Package.With_Package ("Marlowe.Key_Storage", Private_With => True);
      Top_Package.With_Package ("Kit.Mutex", Private_With => True);

      Top_Package.Append
        (Syn.Declarations.New_Full_Type_Declaration
           ("Integer_64",
            Syn.Types.New_Range_Definition
              (Low  => "-2 ** 63",
               High => "2 ** 63 - 1")));

      declare
         Key_Package : Syn.Declarations.Package_Type :=
                         Syn.Declarations.New_Package_Type
                           ("Integer_64_Storage");
      begin
         Key_Package.Set_Generic_Instantiation
           ("Marlowe.Key_Storage.Integral_Storage");
         Key_Package.Add_Generic_Actual_Argument
           ("Integer_64");
         Key_Package.Set_Private_Spec;
         Top_Package.Append (Key_Package);
      end;

      Create_User_Defined_Types (Db, Top_Package);

      --  Create_Handle_Function (Db, Top_Package);
      Create_Table_Type (Db, Top_Package);
--        Create_Key_Type (Db, Top_Package);
--        Create_Field_Type (Db, Top_Package);
      Create_Reference_Types (Db, Top_Package);
      Create_Record_Interface (Db, Top_Package);
      Create_Search_Interface (Db, Top_Package);
      Create_Locking_Interface (Db, Top_Package);
      Top_Package.Append
        (Syn.Declarations.New_Constant_Declaration
           ("Database_Magic_Number",
            Syn.Literal (1)));
      Project.Add_Package (Top_Package);
      Project.Add_Package
        (Database_Package.Generate_Database_Package (Db));
      Project.Add_Package
        (Marlowe_Keys_Package.Generate_Package (Db));
      Project.Add_Package
        (Table_Name_Map.Generate_Package (Db));

      Project.Add_Package
        (Database_Package.Generate_Database_Types_Package (Db));

      Db.Iterate (Table_Database_Package'Access);
      Db.Iterate (Get_From_Cache'Access);
      Db.Iterate (Handle_Interface'Access);
      Db.Iterate (Public_Interface'Access);
      Db.Iterate (Private_Interface'Access);
      return Project;
   end Generate_Database;

   ------------------------
   -- Generated_Database --
   ------------------------

   function Generated_Database return Generated_Database_Type is
   begin
      return Btree_Marlowe;
   end Generated_Database;

   ---------------------
   -- Option_Ada_2022 --
   ---------------------

   function Option_Ada_2022 return Boolean is
   begin
      return Ada_2022_Option;
   end Option_Ada_2022;

   -------------------------------
   -- Option_Deadlock_Detection --
   -------------------------------

   function Option_Deadlock_Detection return Boolean is
   begin
      return Deadlock_Detection_Option;
   end Option_Deadlock_Detection;

   ------------------
   -- Option_Debug --
   ------------------

   function Option_Debug return Boolean is
   begin
      return Debug_Option;
   end Option_Debug;

   -----------------------------
   -- Option_Output_Directory --
   -----------------------------

   function Option_Output_Directory return String is
   begin
      return Ada.Strings.Unbounded.To_String (Output_Directory_Option);
   end Option_Output_Directory;

   -----------------
   -- Set_Options --
   -----------------

   procedure Set_Options
     (Ada_2022           : Boolean := False;
      Deadlock_Detection : Boolean := False;
      Debug              : Boolean := False;
      Output_Directory   : String  := ".")
   is
   begin
      Ada_2022_Option := Ada_2022;
      Deadlock_Detection_Option := Deadlock_Detection;
      Debug_Option := Debug;
      Output_Directory_Option :=
        Ada.Strings.Unbounded.To_Unbounded_String (Output_Directory);
   end Set_Options;

end Kit.Generate;
