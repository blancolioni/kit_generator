with Kit.Schema.Fields;
with Kit.Schema.Keys;
with Kit.Schema.Types;

with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;

package body Kit.Generate.Database_Package is

   type Database_Operation is (Create, Open);

   function Operation_Name (Op : Database_Operation) return String;

   procedure Initialise_Database_Structure
     (Db  : Kit.Schema.Databases.Database_Type;
      Seq : in out Syn.Statement_Sequencer'Class);

   -------------------------------
   -- Generate_Database_Package --
   -------------------------------

   function Generate_Database_Package
     (Db : Kit.Schema.Databases.Database_Type)
      return Syn.Declarations.Package_Type
   is
      Result : Syn.Declarations.Package_Type :=
                 Syn.Declarations.New_Package_Type
                   (Db.Ada_Name & ".Database");

      procedure Add_Implementation_With
        (Table : Kit.Schema.Tables.Table_Type);

      function Create_Database_Procedure
        (Operation : Database_Operation)
         return Syn.Declarations.Subprogram_Declaration'Class;

      function Create_Close_Procedure
        return Syn.Declarations.Subprogram_Declaration'Class;

      -----------------------------
      -- Add_Implementation_With --
      -----------------------------

      procedure Add_Implementation_With
        (Table : Kit.Schema.Tables.Table_Type)
      is
      begin
         Result.With_Package
           (Withed       => Db.Ada_Name & "." & Table.Ada_Name & "_Impl",
            Private_With => False,
            Body_With    => True);
      end Add_Implementation_With;

      ----------------------------
      -- Create_Close_Procedure --
      ----------------------------

      function Create_Close_Procedure
        return Syn.Declarations.Subprogram_Declaration'Class
      is
         Block : Syn.Blocks.Block_Type;
      begin

         Block.Add_Statement
           (Syn.Statements.New_Procedure_Call_Statement
              ("Kit.Notifier.Stop"));
         if Option_Deadlock_Detection then
            Block.Add_Statement
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Kit_Locking.Stop_Scanning"));
         end if;

         Block.Add_Statement
           (Syn.Statements.New_Procedure_Call_Statement
              ("Kit_Deferred_Keys.Close_Deferred_Keys"));
         Block.Add_Statement
           (Syn.Statements.New_Procedure_Call_Statement
              ("Kit.Cache.Close"));
         Block.Add_Statement
           (Syn.Statements.New_Procedure_Call_Statement
              ("Database_Mutex.Lock"));
         Block.Add_Statement
           (Syn.Statements.New_Procedure_Call_Statement
              ("Marlowe_Keys.Handle.Close"));
         Block.Add_Statement
           (Syn.Statements.New_Procedure_Call_Statement
              ("Database_Mutex.Unlock"));
         return Syn.Declarations.New_Procedure ("Close", Block);
      end Create_Close_Procedure;

      -------------------------------
      -- Create_Database_Procedure --
      -------------------------------

      function Create_Database_Procedure
        (Operation : Database_Operation)
         return Syn.Declarations.Subprogram_Declaration'Class
      is
         use Syn.Declarations;
         Block : Syn.Blocks.Block_Type;

         procedure Create_Table
           (Table : Kit.Schema.Tables.Table_Type);

         procedure Open_Table
           (Table : Kit.Schema.Tables.Table_Type);

         ------------------
         -- Create_Table --
         ------------------

         procedure Create_Table
           (Table : Kit.Schema.Tables.Table_Type)
         is
         begin
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Db_" & Table.Ada_Name & ".Create"));
         end Create_Table;

         ----------------
         -- Open_Table --
         ----------------

         procedure Open_Table
           (Table : Kit.Schema.Tables.Table_Type)
         is
         begin
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Db_" & Table.Ada_Name & ".Open"));
         end Open_Table;

         Access_Db  : Syn.Statements.Procedure_Call_Statement :=
                        Syn.Statements.New_Procedure_Call_Statement
                          ("Marlowe_Keys.Handle." &
                           Operation_Name (Operation));
      begin

         Block.Add_Statement
           (Syn.Statements.New_Procedure_Call_Statement
              (Procedure_Name => "Kit.Cache.Start_Cache"));

         Block.Add_Statement
           (Syn.Statements.New_Procedure_Call_Statement
              ("Database_Mutex.Lock"));

         Block.Add_Statement
           (Syn.Statements.New_Assignment_Statement
              (Target => "Marlowe_Keys.Handle",
               Value  =>
                 Syn.Expressions.New_Allocation_Expression
                   (Allocated_Type => Data_Store_Type_Name)));

         Access_Db.Add_Actual_Argument
           (Syn.Object ("Path"));
         Access_Db.Add_Actual_Argument
           (Syn.Object ("Database_Magic_Number"));
         Block.Add_Statement (Access_Db);

         case Operation is
            when Create =>
               Db.Iterate (Create_Table'Access);
            when Open =>
               Db.Iterate (Open_Table'Access);
         end case;

         Block.Add_Statement
           (Syn.Statements.New_Procedure_Call_Statement
              ("Database_Mutex.Unlock"));

         if Operation = Create then
            Initialise_Database_Structure (Db, Block);
         end if;

         if Option_Deadlock_Detection then
            Block.Add_Statement
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Kit_Locking.Start_Scanning"));
         end if;

         declare
            Result : Subprogram_Declaration'Class :=
                       New_Procedure (Operation_Name (Operation),
                                      Block);
         begin
            Result.Add_Formal_Argument
              (Arg_Name    => "Path",
               Arg_Type    => "String",
               Arg_Default =>
                 Syn.Literal (Db.Name & ".marlowe"));

            return Result;
         end;

      end Create_Database_Procedure;

   begin

      Result.With_Package (Data_Store_Package_Name, Body_With => True);

      Result.With_Package ("Kit.Cache", Body_With => True);
      Result.With_Package ("Kit.Notifier", Body_With => True);

      Result.With_Package (Db.Ada_Name & ".Marlowe_Keys",
                           Body_With => True);

      Result.With_Package (Db.Ada_Name & ".Kit_Deferred_Keys",
                           Body_With => True);

      if Option_Deadlock_Detection then
         Result.With_Package (Db.Ada_Name & ".Kit_Locking",
                              Body_With => True);
      end if;

      Result.With_Package
        (Db.Ada_Name & ".Database.Types", Body_With => True);

      declare
         procedure With_Table_Database
           (Table : Kit.Schema.Tables.Table_Type);

         -------------------------
         -- With_Table_Database --
         -------------------------

         procedure With_Table_Database
           (Table : Kit.Schema.Tables.Table_Type)
         is
         begin
            Result.With_Package
              (Db.Ada_Name & ".Database."
               & "Db_" & Table.Ada_Name,
               Body_With => True);
         end With_Table_Database;

      begin
         Db.Iterate (With_Table_Database'Access);
      end;

      if False then
         Db.Iterate (Add_Implementation_With'Access);
      end if;

      for I in Database_Operation loop
         declare
            use Syn.Declarations;
            Proc : constant Subprogram_Declaration'Class :=
                     Create_Database_Procedure (I);
         begin
            Result.Append (Proc);
         end;
      end loop;

      Result.Append (Create_Close_Procedure);

      return Result;
   end Generate_Database_Package;

   -------------------------------------
   -- Generate_Database_Types_Package --
   -------------------------------------

   function Generate_Database_Types_Package
     (Db    : Kit.Schema.Databases.Database_Type)
      return Syn.Declarations.Package_Type
   is
      Result : Syn.Declarations.Package_Type :=
                 Syn.Declarations.New_Package_Type
                   (Db.Ada_Name & ".Database.Types");
      Block  : Syn.Blocks.Block_Type;

      procedure Create_Type (T  : Kit.Schema.Types.Kit_Type);

      -----------------
      -- Create_Type --
      -----------------

      procedure Create_Type (T  : Kit.Schema.Types.Kit_Type) is
      begin
         if not T.Is_Table_Reference
           and then not T.Is_External_Type
         then
            Block.Append (T.Create_Database_Record);
         end if;
      end Create_Type;

   begin

      Result.Set_Private;

      Result.With_Package (Db.Ada_Name & ".Kit_Enumeration",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Float",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Integer",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Long_Integer",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Literal",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Long_Float",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Bounded_String",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Fixed_String",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Type",
                           Body_With => True);

      Kit.Schema.Types.Iterate_All_Types (Create_Type'Access);

      Result.Append
        (Syn.Declarations.New_Procedure ("Create_Types", Block));

      return Result;

   end Generate_Database_Types_Package;

   -------------------------------------
   -- Generate_Table_Database_Package --
   -------------------------------------

   function Generate_Table_Database_Package
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type)
      return Syn.Declarations.Package_Type
   is

      Ref_Name   : constant String :=
                     Table.Ada_Name & "_Ref";

      Result : Syn.Declarations.Package_Type :=
                 Syn.Declarations.New_Package_Type
                   (Db.Ada_Name & ".Database."
                    & "Db_" & Table.Ada_Name);

      procedure Add_Base_With
        (Table : Kit.Schema.Tables.Table_Type);

      function Create_Database_Procedure
        (Operation : Database_Operation)
         return Syn.Declarations.Subprogram_Declaration'Class;

      function Initialize_Table_Procedure
        return Syn.Declarations.Subprogram_Declaration'Class;

      -------------------
      -- Add_Base_With --
      -------------------

      procedure Add_Base_With
        (Table : Kit.Schema.Tables.Table_Type)
      is
      begin
         Result.With_Package
           (Withed       =>
              Db.Ada_Name
            & ".Database.Db_" & Table.Ada_Name,
            Body_With    => True);
      end Add_Base_With;

      -------------------------------
      -- Create_Database_Procedure --
      -------------------------------

      function Create_Database_Procedure
        (Operation : Database_Operation)
         return Syn.Declarations.Subprogram_Declaration'Class
      is
         use Syn.Declarations;
         Block : Syn.Blocks.Block_Type;

         procedure Create_Table;

         procedure Open_Table;

         ------------------
         -- Create_Table --
         ------------------

         procedure Create_Table is

            use Syn.Statements;

            procedure Create_Key
              (Base : Kit.Schema.Tables.Table_Type;
               Key  : Kit.Schema.Keys.Key_Type);

            ----------------
            -- Create_Key --
            ----------------

            procedure Create_Key
              (Base : Kit.Schema.Tables.Table_Type;
               Key  : Kit.Schema.Keys.Key_Type)
            is
               pragma Unreferenced (Base);
               use Syn;
               use Syn.Expressions;
               use Kit.Schema.Tables;

               Call_Add_Key : Function_Call_Expression :=
                                New_Function_Call_Expression
                                  (Procedure_Name =>
                                     "Marlowe_Keys.Handle.Add_Key");
            begin
               Call_Add_Key.Add_Actual_Argument
                 (Literal (Table.Name & "_" & Key.Standard_Name));
               Call_Add_Key.Add_Actual_Argument
                 (Object (Table.Ada_Name & "_Table_Index"));
               Call_Add_Key.Add_Actual_Argument
                 (Literal (Key.Size));

               Block.Add_Statement
                 (New_Assignment_Statement
                    ("Marlowe_Keys."
                     & Table.Key_Reference_Name (Key),
                     Call_Add_Key));
            end Create_Key;

            Proc  : Procedure_Call_Statement :=
                      New_Procedure_Call_Statement
                        ("Marlowe_Keys.Handle.Add_Table");
         begin
            Proc.Add_Actual_Argument
              (Syn.Literal (Table.Standard_Name));
            Proc.Add_Actual_Argument
              (Syn.Literal (Natural (Table.Length)));
            Block.Add_Statement (Proc);

            Table.Scan_Keys (Create_Key'Access);
         end Create_Table;

         ----------------
         -- Open_Table --
         ----------------

         procedure Open_Table is
            use Syn.Statements;

            procedure Open_Key
              (Base : Kit.Schema.Tables.Table_Type;
               Key  : Kit.Schema.Keys.Key_Type);

            --------------
            -- Open_Key --
            --------------

            procedure Open_Key
              (Base : Kit.Schema.Tables.Table_Type;
               Key  : Kit.Schema.Keys.Key_Type)
            is
               pragma Unreferenced (Base);
               use Syn.Expressions;
               use Kit.Schema.Tables;

               Call_Open_Key : Function_Call_Expression :=
                                 New_Function_Call_Expression
                                   (Procedure_Name =>
                                        "Marlowe_Keys.Handle.Get_Reference");
            begin
               Call_Open_Key.Add_Actual_Argument
                 (Syn.Literal
                    (Table.Name & "_" & Key.Standard_Name));

               Block.Add_Statement
                 (New_Assignment_Statement
                    ("Marlowe_Keys."
                     & Table.Key_Reference_Name (Key),
                     Call_Open_Key));
            end Open_Key;

         begin

            Table.Scan_Keys (Open_Key'Access);

         end Open_Table;

      begin

         case Operation is
            when Create =>
               Create_Table;
            when Open =>
               Open_Table;
         end case;

         declare
            Result : constant Subprogram_Declaration'Class :=
                       New_Procedure (Operation_Name (Operation),
                                      Block);
         begin
            return Result;
         end;

      end Create_Database_Procedure;

      --------------------------------
      -- Initialize_Table_Procedure --
      --------------------------------

      function Initialize_Table_Procedure
        return Syn.Declarations.Subprogram_Declaration'Class
      is

         use Syn, Syn.Declarations, Syn.Expressions, Syn.Statements;

         Init_Block : Syn.Blocks.Block_Type;

         procedure Create_Field
           (Field       : Kit.Schema.Fields.Field_Type);

         procedure Create_Base (Base  : Kit.Schema.Tables.Table_Type);

         procedure Create_Key
           (Key : Kit.Schema.Keys.Key_Type);

         procedure Insert_Display_Field
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type);

         -----------------
         -- Create_Base --
         -----------------

         procedure Create_Base (Base  : Kit.Schema.Tables.Table_Type) is
            New_Base : Procedure_Call_Statement'Class :=
                         New_Procedure_Call_Statement
                           ("Kit_Record_Base.Create");
         begin
            New_Base.Add_Actual_Argument
              (Literal (Natural (Table.Base_Index (Base))));
            New_Base.Add_Actual_Argument
              (Object
                 ("Db_" & Base.Ada_Name & "." & Base.Ada_Name & "_Ref"));
            New_Base.Add_Actual_Argument
              (Object (Ref_Name));
            Init_Block.Append (New_Base);
         end Create_Base;

         ------------------
         -- Create_Field --
         ------------------

         procedure Create_Field
           (Field       : Kit.Schema.Fields.Field_Type)
         is
            New_Field : Procedure_Call_Statement'Class :=
                          New_Procedure_Call_Statement
                            ("Kit_Field.Create");
            Block     : Syn.Blocks.Block_Type;
         begin
            Block.Add_Declaration
              (New_Constant_Declaration
                 ("Type_Ref",
                  "Kit_Type_Reference",
                  Field.Get_Field_Type.Reference_Database_Type));
            New_Field.Add_Actual_Argument
              (Literal (Field.Standard_Name));
            New_Field.Add_Actual_Argument
              (Object (Ref_Name));
            New_Field.Add_Actual_Argument
              (Object ("Type_Ref"));
            New_Field.Add_Actual_Argument
              (Literal (Natural (Table.Field_Start (Field))));
            New_Field.Add_Actual_Argument
              (Literal (Field.Get_Field_Type.Size));
            New_Field.Add_Actual_Argument
              (Literal (Field.Created));
            New_Field.Add_Actual_Argument
              (Literal (Field.Readable));
            New_Field.Add_Actual_Argument
              (Literal (Field.Writeable));
            New_Field.Add_Actual_Argument
              (Literal (Field.Display));
            New_Field.Add_Actual_Argument
              (Literal (Field.Base_Reference));
            Block.Add_Statement (New_Field);
            Init_Block.Append (Declare_Statement (Block));
         end Create_Field;

         ----------------
         -- Create_Key --
         ----------------

         procedure Create_Key
           (Key : Kit.Schema.Keys.Key_Type)
         is
            New_Key : Function_Call_Expression'Class :=
                        New_Function_Call_Expression ("Kit_Key.Create");
            Block   : Syn.Blocks.Block_Type;
         begin
            New_Key.Add_Actual_Argument (Literal (Key.Standard_Name));
            New_Key.Add_Actual_Argument
              (Object (Ref_Name));
            New_Key.Add_Actual_Argument
              (Object ((if Key.Unique then "True" else "False")));
            New_Key.Add_Actual_Argument (Literal (Key.Size));
            Block.Add_Declaration
              (New_Constant_Declaration
                 ("Ref", "Kit_Key_Reference",
                  New_Key));
            for I in 1 .. Key.Field_Count loop
               declare
                  Key_Field   : Procedure_Call_Statement'Class :=
                                  New_Procedure_Call_Statement
                                    ("Kit_Key_Field.Create");
                  Field_Block : Syn.Blocks.Block_Type;
               begin
                  Field_Block.Add_Declaration
                    (New_Constant_Declaration
                       ("Field_Ref",
                        "Kit_Field_Reference",
                        New_Function_Call_Expression
                          ("Kit_Field.Get_By_Record_Field",
                           Object (Ref_Name),
                           Literal (Key.Field (I).Standard_Name))));
                  Key_Field.Add_Actual_Argument (Object ("Ref"));
                  Key_Field.Add_Actual_Argument (Object ("Field_Ref"));
                  Field_Block.Add_Statement (Key_Field);
                  Block.Add_Statement
                    (Declare_Statement (Field_Block));
               end;
            end loop;

            Init_Block.Append (Declare_Statement (Block));
         end Create_Key;

         --------------------------
         -- Insert_Display_Field --
         --------------------------

         procedure Insert_Display_Field
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type)
         is
         begin
            if Field.Display then
               Init_Block.Append
                 (New_Procedure_Call_Statement
                    (Procedure_Name => "Kit_Display_Field.Create",
                     Argument_1     =>
                       Object (Table.Ada_Name & "_Ref"),
                     Argument_2     =>
                       New_Function_Call_Expression
                         ("Kit_Field.Get_By_Record_Field",
                          Object ("Db_" & Base.Ada_Name & "."
                            & Base.Ada_Name & "_Ref"),
                          Literal (Field.Standard_Name))));
            end if;
         end Insert_Display_Field;

      begin
         Init_Block.Append
           (New_Assignment_Statement
              (Ref_Name,
               New_Function_Call_Expression
                 ("Kit_Record.Create",
                  Literal (Table.Standard_Name),
                  Object (Table.Index_Image),
                  Literal (Natural (Table.Length)))));

         Init_Block.Append (Table.Reference_Type.Create_Database_Record);
         Table.Scan_Fields (Create_Field'Access);
         Table.Scan_Keys (Create_Key'Access);
         Table.Iterate (Create_Base'Access, Inclusive => False);
         Table.Iterate_All (Insert_Display_Field'Access);

         return Syn.Declarations.New_Procedure
           ("Initialize",
            Init_Block);
      end Initialize_Table_Procedure;

   begin
      Result.Set_Private;

      Result.With_Package (Db.Ada_Name & ".Marlowe_Keys",
                           Body_With => True);

      if Table.Has_Display_Field then
         Result.With_Package (Db.Ada_Name & ".Kit_Display_Field",
                              Body_With => True);
      end if;

      Result.With_Package (Db.Ada_Name & ".Kit_Field",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Key",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Key_Field",
                           Body_With => True);

      Result.With_Package (Db.Ada_Name & ".Kit_Record",
                           Body_With => True);
      if Table.Has_Inherited_Table then
         Result.With_Package (Db.Ada_Name & ".Kit_Record_Base",
                              Body_With => True);
      end if;

      Result.With_Package (Db.Ada_Name & ".Kit_Reference",
                           Body_With => True);
      Result.With_Package (Db.Ada_Name & ".Kit_Type",
                           Body_With => True);
      Table.Iterate (Add_Base_With'Access, Inclusive => False);

      Result.Append
        (Syn.Declarations.New_Object_Declaration
           (Ref_Name,
            "Kit_Record_Reference"));

      Result.Append (Create_Database_Procedure (Create));
      Result.Append (Create_Database_Procedure (Open));
      Result.Append (Initialize_Table_Procedure);

      return Result;

   end Generate_Table_Database_Package;

   -----------------------------------
   -- Initialise_Database_Structure --
   -----------------------------------

   procedure Initialise_Database_Structure
     (Db  : Kit.Schema.Databases.Database_Type;
      Seq : in out Syn.Statement_Sequencer'Class)
   is

      use Syn;
      use Syn.Statements;

      procedure Create_Table_Fields
        (Table : Kit.Schema.Tables.Table_Type);

      Init_Block : Syn.Blocks.Block_Type;

      -------------------------
      -- Create_Table_Fields --
      -------------------------

      procedure Create_Table_Fields
        (Table : Kit.Schema.Tables.Table_Type)
      is
      begin
         Init_Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              (Db.Ada_Name & ".Database.Db_" & Table.Ada_Name
               & ".Initialize"));
      end Create_Table_Fields;

   begin
      Seq.Append
        (Syn.Statements.New_Procedure_Call_Statement
           (Db.Ada_Name & ".Database.Types.Create_Types"));

      Db.Iterate (Create_Table_Fields'Access);
      Seq.Append (Declare_Statement (Init_Block));
   end Initialise_Database_Structure;

   --------------------
   -- Operation_Name --
   --------------------

   function Operation_Name (Op : Database_Operation) return String is
   begin
      case Op is
         when Create =>
            return "Create";
         when Open =>
            return "Open";
      end case;
   end Operation_Name;

end Kit.Generate.Database_Package;
