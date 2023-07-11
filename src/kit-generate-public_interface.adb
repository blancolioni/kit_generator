with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;

with Kit.Schema.Fields;
with Kit.Schema.Keys;
with Kit.Schema.Types;

with Kit.Generate.Fetch;
with Kit.Generate.Public_Get;

with Kit.String_Maps;

package body Kit.Generate.Public_Interface is

   procedure Create_Control_Procedures
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Field_Constants
     (Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Overrides
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Search_Procedures
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Field_Store_Procedure
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Base  : Kit.Schema.Tables.Table_Type;
      Field : Kit.Schema.Fields.Field_Type;
      Top   : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Identity_Function
     (Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Generic_Get
     (Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Generic_Set
     (Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Notification_Handles
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class);

   procedure Perform_X_Lock
     (Sequence          : in out Syn.Statement_Sequencer'Class;
      Table_Name        : String;
      Object_Name       : String;
      Request_Object    : String;
      Index_Object      : String);

   function Apply_To_Record_Indices
     (Table  : Kit.Schema.Tables.Table_Type;
      Object : String;
      Fn     : not null access
        function (Base          : Kit.Schema.Tables.Table_Type;
                  Argument_Name : String)
      return Syn.Statement'Class)
      return Syn.Statements.Sequence_Of_Statements;

   -----------------------------
   -- Apply_To_Record_Indices --
   -----------------------------

   function Apply_To_Record_Indices
     (Table  : Kit.Schema.Tables.Table_Type;
      Object : String;
      Fn     : not null access
        function (Base          : Kit.Schema.Tables.Table_Type;
                  Argument_Name : String)
      return Syn.Statement'Class)
      return Syn.Statements.Sequence_Of_Statements
   is
      Seq : Syn.Statements.Sequence_Of_Statements;

      procedure Apply (Base : Kit.Schema.Tables.Table_Type);

      -----------
      -- Apply --
      -----------

      procedure Apply (Base : Kit.Schema.Tables.Table_Type) is
         Field : constant String :=
           (if Base.Ada_Name = Table.Ada_Name
            then ".M_Index"
            else Table.Base_Component_Name & "." & Base.Ada_Name);
      begin
         Seq.Append
           (Fn (Base, Object & Field));
      end Apply;

   begin
      Table.Iterate (Apply'Access, True);
      return Seq;
   end Apply_To_Record_Indices;

   -------------------------------
   -- Create_Control_Procedures --
   -------------------------------

   procedure Create_Control_Procedures
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions;

      Argument     : constant Formal_Argument'Class :=
                       New_Inout_Argument
                         ("Item",
                          Syn.Named_Subtype
                            (Table.Update_Implementation_Name));

      procedure Create_Delete;
      procedure Create_Initialize;
      procedure Create_Finalize;

      -------------------
      -- Create_Delete --
      -------------------

      procedure Create_Delete is
         use Syn.Statements;
         Delete_Block : Syn.Blocks.Block_Type;
      begin
         Delete_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Lock"));

         Delete_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Item.Deleted := True"));

         Delete_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Unlock"));

         declare
            Delete : Subprogram_Declaration'Class :=
                       New_Procedure
                        ("Delete",
                         New_Inout_Argument ("Item",
                           Syn.Named_Subtype
                             (Table.Update_Implementation_Name)),
                         Delete_Block);
         begin
            Delete.Set_Overriding;
            Top.Append_To_Body (Delete);
         end;

      end Create_Delete;

      ---------------------
      -- Create_Finalize --
      ---------------------

      procedure Create_Finalize is
         use Syn.Statements;
         Insert_Keys : Sequence_Of_Statements;
         Finalize_Block : Syn.Blocks.Block_Type;
         Delete_Key_Statements : Sequence_Of_Statements;

         Notify_Change_Statements : Sequence_Of_Statements;

         procedure Key_Operation
           (Operation_Name : String;
            Target         : in out Sequence_Of_Statements);

         procedure Set_Db_Deleted
           (Base : Kit.Schema.Tables.Table_Type);

         procedure Notify_Change
           (Base : Kit.Schema.Tables.Table_Type);

         procedure Notify_Create
           (Base : Kit.Schema.Tables.Table_Type);

         procedure Notify_Delete
           (Base : Kit.Schema.Tables.Table_Type);

         -------------------
         -- Key_Operation --
         -------------------

         procedure Key_Operation
           (Operation_Name : String;
            Target         : in out Sequence_Of_Statements)
         is

            procedure Operate_Table_Keys
              (Base  : Kit.Schema.Tables.Table_Type);

            ------------------------
            -- Operate_Table_Keys --
            ------------------------

            procedure Operate_Table_Keys
              (Base  : Kit.Schema.Tables.Table_Type)
            is
               procedure Operate_Key
                 (Key_Table : Kit.Schema.Tables.Table_Type;
                  Key       : Kit.Schema.Keys.Key_Type);

               -----------------
               -- Operate_Key --
               -----------------

               procedure Operate_Key
                 (Key_Table : Kit.Schema.Tables.Table_Type;
                  Key       : Kit.Schema.Keys.Key_Type)
               is
                  Insert : Procedure_Call_Statement :=
                             New_Procedure_Call_Statement
                               ("Marlowe_Keys.Handle."
                               & Operation_Name);
               begin
                  Insert.Add_Actual_Argument ("Marlowe_Keys."
                                              & Base.Key_Reference_Name
                                                (Key));
                  Insert.Add_Actual_Argument
                    (Table.To_Storage (Base_Table  => Base,
                                       Key_Table   => Key_Table,
                                       Object_Name => "Item",
                                       Key         => Key,
                                       With_Index  => True));
                  Target.Append (Insert);
               end Operate_Key;

            begin
               Base.Scan_Keys (Operate_Key'Access);
            end Operate_Table_Keys;

         begin
            Table.Iterate (Operate_Table_Keys'Access,
                           Inclusive   => True,
                           Table_First => False);
         end Key_Operation;

         -------------------
         -- Notify_Change --
         -------------------

         procedure Notify_Change
           (Base : Kit.Schema.Tables.Table_Type)
         is
         begin
            Notify_Change_Statements.Append
              (New_Procedure_Call_Statement
                 ("Kit.Notifier.Record_Changed",
                  Value (Base.Index_Image),
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Table.Database_Index_Component
                       ("Item", Base))));
         end Notify_Change;

         -------------------
         -- Notify_Create --
         -------------------

         procedure Notify_Create
           (Base : Kit.Schema.Tables.Table_Type)
         is
         begin
            Insert_Keys.Append
              (New_Procedure_Call_Statement
                 ("Kit.Notifier.Record_Created",
                  Value (Base.Index_Image),
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Table.Database_Index_Component
                       ("Item", Base))));
         end Notify_Create;

         -------------------
         -- Notify_Delete --
         -------------------

         procedure Notify_Delete
           (Base : Kit.Schema.Tables.Table_Type)
         is
         begin
            Delete_Key_Statements.Append
              (New_Procedure_Call_Statement
                 ("Kit.Notifier.Record_Deleted",
                  Value (Base.Index_Image),
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Table.Database_Index_Component
                       ("Item", Base))));
         end Notify_Delete;

         --------------------
         -- Set_Db_Deleted --
         --------------------

         procedure Set_Db_Deleted
           (Base : Kit.Schema.Tables.Table_Type)
         is
         begin
            Delete_Key_Statements.Append
              (New_Assignment_Statement
                 (Target =>
                      "Item.T" & Base.Index_Image & "_Data.Deleted",
                  Value  => Object ("True")));
         end Set_Db_Deleted;

      begin

         Finalize_Block.Add_Declaration
           (Syn.Declarations.Use_Type
              ("System.Storage_Elements.Storage_Array"));

         Finalize_Block.Add_Statement
           (If_Statement
              (Operator ("=", Object ("Item.M_Index"), Literal (0)),
               New_Return_Statement));

         Insert_Keys.Append
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Lock"));

         if Option_Debug then
            Finalize_Block.Add_Statement
              (New_Procedure_Call_Statement
                 ("Ada.Text_IO.Put_Line",
                  Operator
                    ("&",
                     Literal
                       ("Finalize " & Table.Ada_Name & ":"),
                     New_Function_Call_Expression
                       ("To_String",
                        Object ("Item.M_Index")))));
         end if;

         Key_Operation ("Delete", Delete_Key_Statements);

         Table.Iterate
           (Process => Set_Db_Deleted'Access,
            Inclusive => True);

         Table.Iterate
           (Process => Notify_Delete'Access,
            Inclusive => True);

         declare
            Deleted  : constant Expression'Class :=
                         Object ("Item.Deleted");
            If_Deleted : constant Statement'Class :=
                           If_Statement (Deleted, Delete_Key_Statements);
         begin
            Finalize_Block.Add_Statement (If_Deleted);
         end;

         Table.Iterate
           (Process => Notify_Change'Access,
            Inclusive => True);

         declare
            Read_Only      : constant Expression'Class :=
                               Object ("Item.Read_Only");
            Created        : constant Expression'Class :=
              Object ("Item.Created");
            Deleted        : constant Expression'Class :=
              Object ("Item.Deleted");
            Notify         : constant Expression'Class :=
              Operator
                ("not",
                 Operator
                   ("or else",
                    Operator
                      ("or else", Read_Only, Created), Deleted));
            Send_Notify : constant Statement'Class :=
              If_Statement (Notify, Notify_Change_Statements);
         begin
            Finalize_Block.Add_Statement (Send_Notify);
         end;

         Key_Operation ("Insert", Insert_Keys);

         Insert_Keys.Append
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Unlock"));

         Table.Iterate
           (Notify_Create'Access, Inclusive => True);

         declare
            Read_Write  : constant Expression'Class :=
              Operator ("not", Object ("Item.Read_Only"));
            Created     : constant Expression'Class :=
              Object ("Item.Created");
            RW_And_C    : constant Expression'Class :=
              Operator ("and then", Read_Write, Created);
            If_RW_And_C : constant Statement'Class :=
              If_Statement (RW_And_C, Insert_Keys);
         begin
            Finalize_Block.Add_Statement (If_RW_And_C);
         end;

         declare

            function Save_Record
              (Base          : Kit.Schema.Tables.Table_Type;
               Argument_Name : String)
               return Syn.Statement'Class;

            -----------------
            -- Save_Record --
            -----------------

            function Save_Record
              (Base          : Kit.Schema.Tables.Table_Type;
               Argument_Name : String)
               return Syn.Statement'Class
            is
               T_Access : constant String :=
                            "T" & Base.Index_Image & "_Access";
               Block    : Syn.Blocks.Block_Type;
            begin
               Block.Add_Declaration
                 (New_Constant_Declaration
                    (T_Access,
                     Base.Ada_Name & "_Cache.Cache_Access",
                     New_Function_Call_Expression
                       (Base.Ada_Name & "_Cache.Get",
                        New_Function_Call_Expression
                          ("Marlowe.Database_Index",
                           Argument_Name))));
               Block.Add_Statement
                 (New_Assignment_Statement
                    (T_Access & ".Db",
                     Object ("Item.T" & Base.Index_Image & "_Data")));

               return Declare_Statement (Block);
            end Save_Record;

            Read_Write : constant Expression'Class :=
                           Operator ("not", Object ("Item.Read_Only"));

            Save          : constant Syn.Statements.Sequence_Of_Statements :=
                              Apply_To_Record_Indices
                                (Table, "Item", Save_Record'Access);
            If_Do_Unlock  : constant Statement'Class :=
                              If_Statement (Read_Write, Save);
         begin
            Finalize_Block.Add_Statement (If_Do_Unlock);
         end;

         declare

            function Unlock_Table
              (Base          : Kit.Schema.Tables.Table_Type;
               Argument_Name : String)
               return Syn.Statement'Class;

            ------------------
            -- Unlock_Table --
            ------------------

            function Unlock_Table
              (Base          : Kit.Schema.Tables.Table_Type;
               Argument_Name : String)
               return Syn.Statement'Class
            is
            begin
               return New_Procedure_Call_Statement
                 (Base.Ada_Name & "_Cache.Unlock",
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Argument_Name));
            end Unlock_Table;

            Read_Write : constant Expression'Class :=
              Operator ("not", Object ("Item.Read_Only"));

            Unlock        : constant Syn.Statements.Sequence_Of_Statements :=
              Apply_To_Record_Indices (Table, "Item", Unlock_Table'Access);
            If_Do_Unlock  : constant Statement'Class :=
                              If_Statement (Read_Write, Unlock);
         begin
            Finalize_Block.Add_Statement (If_Do_Unlock);
         end;

         Finalize_Block.Add_Statement
           (New_Assignment_Statement
              ("Item.M_Index", Literal (0)));
         Finalize_Block.Add_Statement
           (New_Assignment_Statement
              ("Item.Created", Object ("False")));

         if Option_Debug then
            Finalize_Block.Add_Statement
              (New_Procedure_Call_Statement
                 ("Ada.Text_IO.Put_Line",
                  Literal
                    ("Finalize " & Table.Ada_Name & ": complete")));
         end if;
         declare
            Finalize : Subprogram_Declaration'Class :=
                         New_Procedure ("Finalize",
                                        Argument, Finalize_Block);
         begin
            Finalize.Set_Overriding;
            Top.Append_To_Body (Finalize);
         end;
      end Create_Finalize;

      -----------------------
      -- Create_Initialize --
      -----------------------

      procedure Create_Initialize is
         Initialize_Block : Syn.Blocks.Block_Type;

         procedure Set_Field (Field_Name : String;
                              Value      : String);

         ---------------
         -- Set_Field --
         ---------------

         procedure Set_Field (Field_Name : String;
                              Value      : String)
         is
         begin
            Initialize_Block.Add_Statement
              (Syn.Statements.New_Assignment_Statement
                 ("Item." & Field_Name,
                  Syn.Object (Value)));
         end Set_Field;

      begin

         Set_Field ("Finished", "True");
         Set_Field ("Forward", "True");
         Set_Field ("Read_Only", "True");
         Set_Field ("Created", "False");
         Set_Field ("Deleted", "False");
         Set_Field ("Scanning", "False");
         Set_Field ("Has_Finish", "False");
         Set_Field ("Using_Key", "False");

         Set_Field ("M_Index", "0");
         Set_Field ("Read_Only", "True");

         declare
            Initialize : Subprogram_Declaration'Class :=
                           New_Procedure ("Initialize",
                                          Argument, Initialize_Block);
         begin
            Initialize.Set_Overriding;
            Top.Append_To_Body (Initialize);
         end;
      end Create_Initialize;

   begin
      Create_Initialize;
      Create_Finalize;
      Create_Delete;
   end Create_Control_Procedures;

   ----------------------------
   -- Create_Field_Constants --
   ----------------------------

   procedure Create_Field_Constants
     (Table : Kit.Schema.Tables.Table_Type;
      Top : in out Syn.Declarations.Package_Type'Class)
   is
      procedure Add_Field_Constant
        (Base : Kit.Schema.Tables.Table_Type;
         Item : Kit.Schema.Fields.Field_Type);

      ------------------------
      -- Add_Field_Constant --
      ------------------------

      procedure Add_Field_Constant
        (Base : Kit.Schema.Tables.Table_Type;
         Item : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
         Name : constant String := Item.Ada_Name;
         Value : constant String := Item.Standard_Name;
         Defn : constant Syn.Declaration'Class :=
                  Syn.Declarations.New_Constant_Declaration
                    (Name        => "F_" & Name,
                     Object_Type => "String",
                     Value       => Syn.Literal (Value));
      begin
         Top.Append (Defn);
      end Add_Field_Constant;

   begin
      Table.Iterate_All (Add_Field_Constant'Access);
      Top.Append (Syn.Declarations.New_Separator);
   end Create_Field_Constants;

   ----------------------------------
   -- Create_Field_Store_Procedure --
   ----------------------------------

   procedure Create_Field_Store_Procedure
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Base  : Kit.Schema.Tables.Table_Type;
      Field : Kit.Schema.Fields.Field_Type;
      Top   : in out Syn.Declarations.Package_Type'Class)
   is

      Store_Block : Syn.Blocks.Block_Type;

      procedure Create_Abstract_Store;

      procedure Lock_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type);

      procedure Unlock_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type);

      procedure Perform_Lock
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type;
         Lock     : Boolean);

      procedure Record_Key_Change
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type);

      procedure Delete_Old_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type);

      procedure Insert_New_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type);

      procedure Key_Operation
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type;
         Operation : String);

      procedure Release_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type);
      pragma Unreferenced (Release_Key);

      procedure Process_Keys
        (Process : not null access
           procedure (Table_Base : Kit.Schema.Tables.Table_Type;
                      Key_Base : Kit.Schema.Tables.Table_Type;
                      Key        : Kit.Schema.Keys.Key_Type));

      ---------------------------
      -- Create_Abstract_Store --
      ---------------------------

      procedure Create_Abstract_Store is
         use Syn.Declarations;
         Store : Subprogram_Declaration'Class :=
                   New_Abstract_Procedure
                     ("Set_" & Field.Ada_Name,
                      New_Inout_Argument ("Item",
                        Syn.Named_Subtype
                          (Table.Update_Interface_Name)));
      begin
         Store.Add_Formal_Argument
           (New_Formal_Argument ("Value",
            Syn.Named_Subtype
              (Field.Get_Field_Type.Argument_Subtype)));

         Top.Append (Store);

         if Field.Get_Field_Type.Is_Table_Reference then
            declare
               Type_Name : constant String :=
                             Field.Get_Field_Type.Referenced_Table_Name;
               Store : Subprogram_Declaration'Class :=
                         New_Abstract_Procedure
                           ("Set_" & Field.Ada_Name,
                            New_Inout_Argument ("Item",
                              Syn.Named_Subtype
                                (Table.Update_Interface_Name)));
            begin
               Store.Add_Formal_Argument
                 (New_Formal_Argument ("Value",
                  Syn.Named_Subtype
                    (Db.Ada_Name & "." & Type_Name & "."
                     & Type_Name & "_Type")));

               Top.Append (Store);
            end;
         end if;

         Top.Append (New_Separator);
      end Create_Abstract_Store;

      --------------------
      -- Delete_Old_Key --
      --------------------

      procedure Delete_Old_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type)
      is
      begin
         Key_Operation (Table_Base, Key_Base, Key, "Delete");
      end Delete_Old_Key;

      --------------------
      -- Insert_New_Key --
      --------------------

      procedure Insert_New_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type)
      is
      begin
         Key_Operation (Table_Base, Key_Base, Key, "Insert");
      end Insert_New_Key;

      -------------------
      -- Key_Operation --
      -------------------

      procedure Key_Operation
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type;
         Operation : String)
      is
         S : Syn.Statements.Procedure_Call_Statement :=
               Syn.Statements.New_Procedure_Call_Statement
                 ("Marlowe_Keys.Handle." & Operation);
         Key_Storage : constant Syn.Expression'Class :=
                         Table.To_Storage
                           (Base_Table  => Table_Base,
                            Key_Table   => Key_Base,
                            Object_Name => "Item",
                            Key         => Key,
                            With_Index  => False);
      begin

         S.Add_Actual_Argument
           ("Marlowe_Keys."
            & Table_Base.Key_Reference_Name (Key));
         S.Add_Actual_Argument
           (Syn.Expressions.Operator
              ("&",
               Key_Storage,
               Syn.Expressions.New_Function_Call_Expression
                 ("Marlowe.Key_Storage.To_Storage_Array",
                  Syn.Expressions.New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Table.Database_Index_Component ("Item", Table_Base)))));

         Store_Block.Add_Statement
           (Syn.Statements.If_Statement
              (Syn.Expressions.Operator
                 ("not", Syn.Object ("Item.Created")),
               S));
      end Key_Operation;

      --------------
      -- Lock_Key --
      --------------

      procedure Lock_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type)
      is
      begin
         Perform_Lock (Table_Base, Key_Base, Key, True);
      end Lock_Key;

      ------------------
      -- Perform_Lock --
      ------------------

      procedure Perform_Lock
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type;
         Lock     : Boolean)
      is
         pragma Unreferenced (Key_Base);
         Lock_Name : constant String :=
                       (if Lock then "Lock" else "Unlock");
         S : constant Syn.Statement'Class :=
               Syn.Statements.New_Procedure_Call_Statement
                 (Table_Base.Ada_Name
                  & "_Impl." & Key.Ada_Name
                  & "_Key_Mutex." & Lock_Name);
      begin
         Store_Block.Add_Statement
           (Syn.Statements.If_Statement
              (Syn.Expressions.Operator
                 ("not", Syn.Object ("Item.Created")),
               S));
      end Perform_Lock;

      ------------------
      -- Process_Keys --
      ------------------

      procedure Process_Keys
        (Process : not null access
           procedure (Table_Base : Kit.Schema.Tables.Table_Type;
                      Key_Base : Kit.Schema.Tables.Table_Type;
                      Key        : Kit.Schema.Keys.Key_Type))
      is

         procedure Process_Base
           (Base : Kit.Schema.Tables.Table_Type);

         ------------------
         -- Process_Base --
         ------------------

         procedure Process_Base
           (Base : Kit.Schema.Tables.Table_Type)
         is
         begin
            Base.Scan_Keys (Field, Process);
         end Process_Base;

      begin
         Table.Iterate (Process_Base'Access,
                        Inclusive   => True,
                        Table_First => False);
      end Process_Keys;

      -----------------------
      -- Record_Key_Change --
      -----------------------

      procedure Record_Key_Change
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type)
      is
         S : Syn.Statements.Procedure_Call_Statement :=
               Syn.Statements.New_Procedure_Call_Statement
                 ("Kit_Deferred_Keys.Key_Changed");
         Old_Key_Storage : constant Syn.Expression'Class :=
                             Table.To_Storage
                               (Base_Table  => Table_Base,
                                Key_Table   => Key_Base,
                                Object_Name => "Item",
                                Key         => Key,
                                With_Index  => False);
         New_Key_Storage : constant Syn.Expression'Class :=
                             Table.To_Storage
                               (Base_Table  => Table_Base,
                                Key_Table   => Key_Base,
                                Object_Name => "Item",
                                Key         => Key,
                                New_Field   => Field,
                                Field_Value => "Value",
                                With_Index  => False);
      begin
         S.Add_Actual_Argument
           (Key_Base.Index_Image);
         S.Add_Actual_Argument
           ("Marlowe_Keys."
            & Table_Base.Key_Reference_Name (Key));
         S.Add_Actual_Argument
           (Syn.Expressions.New_Function_Call_Expression
              ("Marlowe.Database_Index",
               Table.Database_Index_Component ("Item", Table_Base)));
         S.Add_Actual_Argument (Old_Key_Storage);
         S.Add_Actual_Argument (New_Key_Storage);

         Store_Block.Add_Statement
           (Syn.Statements.If_Statement
              (Syn.Expressions.Operator
                 ("not", Syn.Object ("Item.Created")),
               S));
      end Record_Key_Change;

      -----------------
      -- Release_Key --
      -----------------

      procedure Release_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type)
      is
         pragma Unreferenced (Key_Base);
         use Syn;
         use Syn.Expressions;
         use Syn.Statements;
         Not_Created    : constant Expression'Class :=
                            Operator
                              ("not", Syn.Object ("Item.Created"));
         Using_Key      : constant Expression'Class :=
                            Object ("Item.Scanning");
         Key_Ref        : constant String :=
                            "Marlowe_Keys."
                            & Table_Base.Key_Reference_Name (Key);
         This_Key       : constant Expression'Class :=
                            Operator
                              ("=",
                               Object ("Item.Key_Ref"),
                               Object (Key_Ref));
         Release        : constant Statement'Class :=
                            New_Procedure_Call_Statement
                              ("Item.Mark.Release");
      begin
         Store_Block.Add_Statement
           (If_Statement
              (Long_Operator
                 ("and then",
                  Operator ("and then", Not_Created, Using_Key),
                  This_Key),
               Release));

      end Release_Key;

      ----------------
      -- Unlock_Key --
      ----------------

      procedure Unlock_Key
        (Table_Base : Kit.Schema.Tables.Table_Type;
         Key_Base   : Kit.Schema.Tables.Table_Type;
         Key        : Kit.Schema.Keys.Key_Type)
      is
      begin
         Perform_Lock (Table_Base, Key_Base, Key, False);
      end Unlock_Key;

   begin

      if not Table.Inherited_Field (Field) then
         Create_Abstract_Store;
      end if;

--        Store_Block.Add_Declaration
--          (Syn.Declarations.Use_Type
--             ("System.Storage_Elements.Storage_Array"));
--        if Table.Has_Key_Field then
--           Store_Block.Add_Declaration
--             (Syn.Declarations.Use_Type
--                ("Marlowe.Data_Stores.Key_Reference"));
--        end if;

      Store_Block.Add_Declaration
        (Syn.Declarations.Renaming_Declaration
           ("Target",
            Field.Get_Field_Type.Unconstrained_Record_Subtype,
            Syn.Object
              (Table.Base_Field_Name ("Item", Base, Field))));

      declare
         use Syn.Statements;
      begin
         Store_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Lock"));

         Store_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Item.X_Lock"));
      end;

      if False then
         Process_Keys (Lock_Key'Access);
      end if;

      --  Table.Scan_Keys (Field, Release_Key'Access);

      Process_Keys (Record_Key_Change'Access);

      if False then
         Process_Keys (Delete_Old_Key'Access);
      end if;

      Field.Get_Field_Type.Set_Value
        (Target_Name => "Target",
         Value_Name  => "Value",
         Sequence    => Store_Block);

      if False then
         Process_Keys (Insert_New_Key'Access);
      end if;

      declare
         use Syn.Statements;
      begin
         Store_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Database_Mutex.Shared_Unlock"));
      end;

      if False then
         Process_Keys (Unlock_Key'Access);
      end if;

      declare
         use Syn.Declarations;
         Store : Subprogram_Declaration'Class :=
                   New_Procedure
                     ("Set_" & Field.Ada_Name,
                      New_Inout_Argument ("Item",
                        Syn.Named_Subtype
                          (Table.Update_Implementation_Name)),
                      Store_Block);
      begin
         Store.Add_Formal_Argument
           (New_Formal_Argument ("Value",
            Syn.Named_Subtype
              (Field.Get_Field_Type.Argument_Subtype)));
         Store.Set_Overriding;

         Top.Append_To_Body (Store);
      end;

      if Field.Get_Field_Type.Is_Table_Reference then
         declare
            use Syn;
            use Syn.Declarations;
            use Syn.Statements;
            Type_Name : constant String :=
                          Field.Get_Field_Type.Referenced_Table_Name;
            Store     : Subprogram_Declaration'Class :=
                          New_Procedure
                            ("Set_" & Field.Ada_Name,
                             New_Inout_Argument ("Item",
                               Syn.Named_Subtype
                                 (Table.Update_Implementation_Name)),
                             Syn.Blocks.Create_Block
                               (New_Procedure_Call_Statement
                                  ("Item.Set_" & Field.Ada_Name,
                                   Object ("Value.Get_"
                                     & Field.Get_Field_Type.Ada_Name
                                     & "_Reference"))));
         begin
            Store.Add_Formal_Argument
              (New_Formal_Argument ("Value",
               Syn.Named_Subtype
                 (Db.Ada_Name & "." & Type_Name & "."
                  & Type_Name & "_Type")));
            Store.Set_Overriding;
            Top.Append_To_Body (Store);
         end;
      end if;

   end Create_Field_Store_Procedure;

   ------------------------
   -- Create_Generic_Get --
   ------------------------

   procedure Create_Generic_Get
     (Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class)
   is
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions;
      use Syn.Statements;

      Got_Field : Boolean := False;

      Block  : Syn.Blocks.Block_Type;
      Choose : If_Statement_Record'Class :=
                 If_Statement
                   (Operator ("=", Object ("Field"), Literal ("")),
                    Raise_Statement
                      ("Constraint_Error",
                       "missing field name"),
                    Raise_Statement
                      ("Constraint_Error",
                       "no such field"));

      procedure Select_Field
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      ------------------
      -- Select_Field --
      ------------------

      procedure Select_Field
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
         Seq : Sequence_Of_Statements;
      begin
         if Field.Readable then
            Seq.Append
              (New_Return_Statement
                 (Field.Get_Field_Type.Convert_To_String
                      ("Item." & Field.Ada_Name)));
            Got_Field := True;
         else
            Seq.Append
              (Raise_Statement
                 ("Constraint_Error",
                  "field " & Field.Ada_Name
                  & " is not readable"));
         end if;

         Choose.Add_Elsif
           (Operator
              ("=", Object ("Field"), Literal (Field.Standard_Name)),
            Seq);

      end Select_Field;

   begin

      Table.Iterate_All (Select_Field'Access);

      if not Got_Field then
         Block.Add_Declaration (New_Pragma ("Unreferenced", "Item"));
      end if;

      Block.Append (Choose);

      declare
         Fetch : Subprogram_Declaration'Class :=
                   New_Function
                     ("Get",
                      Named_Subtype ("String"),
                      Block);
      begin

         Fetch.Add_Formal_Argument
           ("Item",
            Table.Ada_Name & "_Implementation");
         Fetch.Add_Formal_Argument
           ("Field",
            "String");

         Fetch.Set_Overriding;
         Top.Append_To_Body (Fetch);

      end;

   end Create_Generic_Get;

   ------------------------
   -- Create_Generic_Set --
   ------------------------

   procedure Create_Generic_Set
     (Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class)
   is
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions;
      use Syn.Statements;

      Got_Field : Boolean := False;

      Block  : Syn.Blocks.Block_Type;
      Choose : If_Statement_Record'Class :=
                 If_Statement
                   (Operator ("=", Object ("Field"), Literal ("")),
                    Raise_Statement
                      ("Constraint_Error",
                       "missing field name"),
                    Raise_Statement
                      ("Constraint_Error",
                       "no such field"));

      procedure Set_Field
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
         Seq : Sequence_Of_Statements;
      begin
         if Field.Writeable then
            Seq.Append
              (New_Procedure_Call_Statement
                 ("Item.Set_" & Field.Ada_Name,
                  Field.Get_Field_Type.Convert_From_String ("Value")));
            Got_Field := True;
         else
            Seq.Append
              (Raise_Statement
                 ("Constraint_Error",
                  "field " & Field.Ada_Name
                  & " is not writable"));
         end if;

         Choose.Add_Elsif
           (Operator
              ("=", Object ("Field"), Literal (Field.Standard_Name)),
            Seq);

      end Set_Field;

   begin

      Table.Iterate_All (Set_Field'Access);

      Block.Append (Choose);

      if not Got_Field then
         Block.Add_Declaration (New_Pragma ("Unreferenced", "Item"));
         Block.Add_Declaration (New_Pragma ("Unreferenced", "Value"));
      end if;

      declare
         Store : Subprogram_Declaration'Class :=
                   New_Procedure
                     ("Set",
                      Block);
      begin

         Store.Add_Formal_Argument
           ("Item",
            Inout_Argument,
            Table.Update_Implementation_Name);
         Store.Add_Formal_Argument
           ("Field",
            "String");
         Store.Add_Formal_Argument
           ("Value",
            "String");

         Store.Set_Overriding;
         Top.Append_To_Body (Store);
      end;

   end Create_Generic_Set;

   ------------------------------
   -- Create_Identity_Function --
   ------------------------------

   procedure Create_Identity_Function
     (Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class)
   is
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions;
      use Syn.Statements;

      Block  : Syn.Blocks.Block_Type;

   begin

      Block.Add_Statement
        (New_Return_Statement
           (Operator
              ("&",
               Literal (Table.Name),
               New_Function_Call_Expression
                 (Table.Ada_Name & "_Reference'Image",
                  "Item.Get_" & Table.Ada_Name & "_Reference"))));

      declare
         Identity : Subprogram_Declaration'Class :=
                   New_Function
                     ("Identity",
                      Named_Subtype ("String"),
                      Block);
      begin

         Identity.Add_Formal_Argument
           ("Item",
            Table.Ada_Name & "_Implementation");

         Identity.Set_Overriding;
         Top.Append_To_Body (Identity);

      end;

   end Create_Identity_Function;

   ---------------------------------
   -- Create_Notification_Handles --
   ---------------------------------

   procedure Create_Notification_Handles
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Syn, Syn.Declarations;

      Table_Change_Type : Syn.Types.Record_Type_Definition;
      Record_Change_Type : Syn.Types.Record_Type_Definition;
      Table_Change_Type_Name : constant String :=
                          Table.Ada_Name & "_Table_Notify_Record";
      Record_Change_Type_Name : constant String :=
                                  Table.Ada_Name & "_Notify_Record";
      Table_Signature : constant Subprogram_Declaration'Class :=
                          New_Abstract_Procedure ("");
      Record_Signature : constant Subprogram_Declaration'Class :=
                    New_Abstract_Procedure
                             ("",
                              New_Formal_Argument
                                ("Reference",
                                 Syn.Named_Subtype
                                   (Table.Reference_Type_Name)));

      Record_Notifier_Name : constant String :=
                               Table.Ada_Name & "_Notify_Handler";
      Table_Notifier_Name : constant String :=
                              Table.Ada_Name & "_Table_Notify_Handler";

   begin

      Table_Change_Type.Set_Tagged;
      Table_Change_Type.Add_Parent
        ("Kit.Notifier.Table_Notify_Interface");
      Table_Change_Type.Add_Component
        ("Handler", Table_Notifier_Name);

      Top.Append_To_Body
        (New_Full_Type_Declaration
           (Table_Change_Type_Name,
            Table_Change_Type));

      declare
         Block : Syn.Blocks.Block_Type;
      begin

         Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Handle.Handler.all"));

         declare
            Notify : Subprogram_Declaration'Class :=
                       New_Procedure
                         ("Notify_Table_Change",
                          New_Formal_Argument
                            ("Handle",
                             Named_Subtype (Table_Change_Type_Name)),
                          Block);
         begin
            Notify.Set_Overriding;
            Top.Append_To_Body (Notify);
         end;
      end;

      Record_Change_Type.Set_Tagged;
      Record_Change_Type.Add_Parent
        ("Kit.Notifier.Record_Notify_Interface");
      Record_Change_Type.Add_Component
        ("Handler", Record_Notifier_Name);

      Top.Append_To_Body
        (New_Full_Type_Declaration
           (Record_Change_Type_Name,
            Record_Change_Type));

      declare
         Block : Syn.Blocks.Block_Type;
      begin

         Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Handle.Handler",
               Syn.Expressions.New_Function_Call_Expression
                 (Table.Reference_Type_Name,
                  Object ("Changed_Record"))));

         declare
            Notify : Subprogram_Declaration'Class :=
                       New_Procedure
                         ("Notify_Record_Change",
                          New_Formal_Argument
                            ("Handle",
                             Named_Subtype (Record_Change_Type_Name)),
                          New_Formal_Argument
                            ("Changed_Record",
                             Named_Subtype ("Marlowe.Database_Index")),
                          Block);
         begin
            Notify.Set_Overriding;
            Top.Append_To_Body (Notify);
         end;
      end;

      Top.Append (Syn.Declarations.New_Separator);

      Top.Append
        (Syn.Declarations.New_Full_Type_Declaration
           (Record_Notifier_Name,
            Syn.Types.New_Subprogram_Type_Definition
              (Record_Signature)));

      Top.Append
        (Syn.Declarations.New_Full_Type_Declaration
           (Table_Notifier_Name,
            Syn.Types.New_Subprogram_Type_Definition
              (Table_Signature)));

      Top.Append (Syn.Declarations.New_Separator);

      declare
         type On_Change_Type is (Table_Change, Record_Change,
                                 Record_Created, Record_Deleted);
      begin
         for Change in On_Change_Type loop
            declare
               Block : Syn.Blocks.Block_Type;
               Add_Handler_Name : constant String :=
                                    (case Change is
                                        when Table_Change =>
                                           "Add_Table_Change_Handler",
                                        when Record_Change =>
                                           "Add_Record_Change_Handler",
                                        when Record_Created =>
                                           "Add_Record_Create_Handler",
                                        when Record_Deleted =>
                                           "Add_Record_Delete_Handler");
            begin
               Block.Add_Declaration
                 (New_Object_Declaration
                    ("Rec",
                     Named_Subtype
                       (if Change = Table_Change
                        then Table_Change_Type_Name
                        else Record_Change_Type_Name)));
               Block.Append
                 (Syn.Statements.New_Assignment_Statement
                    ("Rec.Handler", Object ("Handler")));
               if Change /= Record_Change then
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Kit.Notifier." & Add_Handler_Name,
                        Value (Table.Index_Image),
                        Object ("Rec")));
               else
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Kit.Notifier." & Add_Handler_Name,
                        Value (Table.Index_Image),
                        Syn.Expressions.New_Function_Call_Expression
                          ("Marlowe.Database_Index",
                           Object ("Reference")),
                        Object ("Rec")));
               end if;

               declare
                  Operation_Name : constant String :=
                                     (case Change is
                                         when Table_Change   =>
                                            "Table_Change",
                                         when Record_Change  =>
                                            "Change",
                                         when Record_Created =>
                                            "Created",
                                         when Record_Deleted =>
                                            "Deleted");

                  Name : constant String :=
                           "On_" & Table.Ada_Name
                           & "_"
                           & Operation_Name;
                  Proc : Subprogram_Declaration'Class :=
                           New_Procedure (Name, Block);
               begin
                  if Change = Record_Change then
                     Proc.Add_Formal_Argument
                       (New_Formal_Argument
                          ("Reference",
                           Named_Subtype (Table.Reference_Type_Name)));
                  end if;
                  Proc.Add_Formal_Argument
                    (New_Formal_Argument
                       ("Handler",
                        Named_Subtype
                          (if Change = Table_Change
                           then Table_Notifier_Name
                           else Record_Notifier_Name)));
                  Top.Append (Proc);
               end;
            end;
         end loop;
      end;
   end Create_Notification_Handles;

   ----------------------
   -- Create_Overrides --
   ----------------------

   procedure Create_Overrides
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Syn.Declarations;
      use Syn.Expressions;
      use Syn.Statements;

      function Lock
        (Base          : Kit.Schema.Tables.Table_Type;
         Argument_Name : String)
         return Syn.Statement'Class
      is (New_Procedure_Call_Statement
          (Base.Ada_Name & "_Cache.X_Lock",
           New_Function_Call_Expression
             ("Marlowe.Database_Index",
              Argument_Name)));

      Locks : constant Syn.Statements.Sequence_Of_Statements :=
        Apply_To_Record_Indices
          (Table, "Item", Lock'Access);
      X_Lock : Subprogram_Declaration'Class :=
        New_Procedure
          ("X_Lock",
           Syn.Blocks.Create_Block
             (Locks));
   begin
      X_Lock.Add_Formal_Argument
        ("Item",
         In_Argument,
         Table.Ada_Name & "_Implementation");
      X_Lock.Set_Overriding;
      Top.Append_To_Body (X_Lock);
   end Create_Overrides;

   ------------------------------
   -- Create_Search_Procedures --
   ------------------------------

   procedure Create_Search_Procedures
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : in out Syn.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions;

      procedure Create_Has_Element;
      procedure Create_Next;
      pragma Unreferenced (Create_Next);

      ------------------------
      -- Create_Has_Element --
      ------------------------

      procedure Create_Has_Element is

         Has_Element_Block : Syn.Blocks.Block_Type;
         --  Next_Block        : Syn.Blocks.Block_Type;
      begin
         Has_Element_Block.Add_Statement
           (Syn.Statements.New_Return_Statement
              (Operator ("/=", Object ("Item.M_Index"),
               Object ("0"))));

         declare
            Has_Element : Subprogram_Declaration'Class :=
                            New_Function
                              ("Has_Element",
                               "Boolean",
                               Has_Element_Block);
         begin
            Has_Element.Add_Formal_Argument
              ("Item",
               Table.Ada_Name & "_Implementation");
            Has_Element.Set_Overriding;
            Top.Append_To_Body (Has_Element);
         end;
      end Create_Has_Element;

      -----------------
      -- Create_Next --
      -----------------

      procedure Create_Next is
         use Syn.Statements;
         Next_Block        : Syn.Blocks.Block_Type;
      begin
         Next_Block.Add_Declaration
           (Use_Type ("Marlowe.Database_Index"));
         Next_Block.Add_Declaration
           (New_Object_Declaration
              ("Got_Valid_Index", "Boolean"));

         Next_Block.Add_Statement
           (If_Statement
              (Operator
                 ("not",
                  Object ("Item.Scanning")),
               Raise_Statement
                 ("Constraint_Error",
                  "Not currently scanning this table")));

         Next_Block.Add_Statement (Table.Ada_Name & "_Impl.File_Mutex"
                                     & ".Shared_Lock");
         Next_Block.Add_Statement ("Item.Local.Unlock");

         Next_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("Item.Mark.Next"));
         Next_Block.Add_Statement
           (New_Assignment_Statement
              ("Got_Valid_Index",
               Object ("Item.Mark.Valid")));
         Next_Block.Add_Statement
           (If_Statement
              (Object ("Got_Valid_Index"),
               New_Assignment_Statement
                 ("Item.M_Index",
                  New_Function_Call_Expression
                    ("Marlowe.Key_Storage.To_Database_Index",
                     Object ("Item.Mark.Get_Key")))));

         Next_Block.Add_Statement (Table.Ada_Name & "_Impl.File_Mutex"
                                   & ".Shared_Unlock");

         declare
            Fetch_Found : Sequence_Of_Statements;
            Not_Found   : Sequence_Of_Statements;
         begin
            Fetch.Fetch_From_Index (Table, "Item", False, Fetch_Found);
            Not_Found.Append ("Item.M_Index := 0");
            Next_Block.Add_Statement
              (If_Statement
                 (Object ("Got_Valid_Index"),
                  Fetch_Found,
                  Not_Found));
         end;

         declare
            Next : Subprogram_Declaration'Class :=
                            New_Procedure
                              ("Next",
                               Next_Block);
         begin
            Next.Add_Formal_Argument
              ("Item",
               Inout_Argument,
               Table.Ada_Name & "_Implementation");
            Next.Set_Overriding;
            Top.Append_To_Body (Next);
         end;
      end Create_Next;

   begin
      Create_Has_Element;
      --  Create_Next;
   end Create_Search_Procedures;

   -------------------------------
   -- Generate_Public_Interface --
   -------------------------------

   function Generate_Public_Interface
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : Syn.Declarations.Package_Type'Class)
      return Syn.Declarations.Package_Type'Class
   is
      use Syn, Syn.Declarations;
      Database_Package       : constant String :=
                                 Table.Ada_Name & "_Impl";
--        Database_Type          : constant String :=
--                                   Database_Package & "."
--                                     & Table.Ada_Name & "_Database_Record";
      Cache_Package          : constant String :=
                                 Table.Ada_Name & "_Cache";
      Implementation_Type    : constant String := Table.Implementation_Name;
      Update_Type            : constant String :=
                                 Table.Update_Implementation_Name;

      Withed_Tables : Kit.String_Maps.String_Map;

      Table_Package : Syn.Declarations.Package_Type'Class :=
                        Top.New_Child_Package (Table.Ada_Name);

      Table_Interface        : Syn.Interface_Type_Definition;
      Table_Update_Interface : Syn.Interface_Type_Definition;

      procedure Add_Field_Type_With
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      procedure Add_Base_With (It : Kit.Schema.Tables.Table_Type);
      procedure Add_Base (It : Kit.Schema.Tables.Table_Type);
      procedure Add_Update_Base (It : Kit.Schema.Tables.Table_Type);

      procedure Add_Fetch (Base  : Kit.Schema.Tables.Table_Type;
                           Field : Kit.Schema.Fields.Field_Type);
      procedure Add_Store (Base  : Kit.Schema.Tables.Table_Type;
                           Field : Kit.Schema.Fields.Field_Type);

      procedure Create_Key_Get
        (Base  : Kit.Schema.Tables.Table_Type;
         Key   : Kit.Schema.Keys.Key_Type);
      procedure Create_Reference_Get
        (Base : Kit.Schema.Tables.Table_Type);

      procedure Add_Create_Function;

      procedure Create_Implementation_Type;

      procedure Create_Update_Type;

      procedure Create_Selection_Type;

      --------------
      -- Add_Base --
      --------------

      procedure Add_Base (It : Kit.Schema.Tables.Table_Type) is
      begin
         Table_Interface.Add_Parent
           (Db.Ada_Name & "." & It.Ada_Name & "." &
              It.Ada_Name & "_Interface");
      end Add_Base;

      -------------------
      -- Add_Base_With --
      -------------------

      procedure Add_Base_With (It : Kit.Schema.Tables.Table_Type) is
      begin
         Table_Package.With_Package
           (Db.Ada_Name & "." & It.Ada_Name,
            Body_With =>  False);
         Table_Package.With_Package
           (Db.Ada_Name & "." & It.Ada_Name & "_Cache",
            Body_With =>  True);
         Table_Package.With_Package
           (Db.Ada_Name & "." & It.Ada_Name & "_Impl",
            Body_With =>  True);
         Withed_Tables.Insert (It.Ada_Name);
      end Add_Base_With;

      -------------------------
      -- Add_Create_Function --
      -------------------------

      procedure Add_Create_Function is

         use Syn.Statements;

         Sequence : Sequence_Of_Statements;

         procedure Add_Create_Arguments
           (Sub : in out Subprogram_Declaration'Class);

         procedure Allocate_Context
           (Base : Kit.Schema.Tables.Table_Type);

         procedure Database_Insert
           (Base : Kit.Schema.Tables.Table_Type);

         procedure Copy_Record
           (Base : Kit.Schema.Tables.Table_Type);

         procedure Set_Field (Field_Name : String;
                              Value      : String);

         Create_Ref_Block : Syn.Blocks.Block_Type;
         Got_Field        : Boolean := False;

         procedure Initialise_Field
           (Base     : Kit.Schema.Tables.Table_Type;
            Field    : Kit.Schema.Fields.Field_Type);

         function Local_Base
           (Base : Kit.Schema.Tables.Table_Type)
            return String
         is ("T" & Base.Index_Image & "_Access");

         --------------------------
         -- Add_Create_Arguments --
         --------------------------

         procedure Add_Create_Arguments
           (Sub : in out Subprogram_Declaration'Class)
         is
            procedure Add_Formal_Argument
              (Base     : Kit.Schema.Tables.Table_Type;
               Field    : Kit.Schema.Fields.Field_Type);

            -------------------------
            -- Add_Formal_Argument --
            -------------------------

            procedure Add_Formal_Argument
              (Base     : Kit.Schema.Tables.Table_Type;
               Field    : Kit.Schema.Fields.Field_Type)
            is
               pragma Unreferenced (Base);
            begin
               if Field.Created then
                  Sub.Add_Formal_Argument
                    (Field.Ada_Name,
                     Field.Get_Field_Type.Argument_Subtype);
               end if;
            end Add_Formal_Argument;

         begin
            Table.Iterate_All (Add_Formal_Argument'Access);
         end Add_Create_Arguments;

         ----------------------
         -- Allocate_Context --
         ----------------------

         procedure Allocate_Context
           (Base : Kit.Schema.Tables.Table_Type)
         is
            use Syn.Expressions;
         begin
            Sequence.Append
              (New_Assignment_Statement
                 (Local_Base (Base),
                  New_Allocation_Expression
                    (Base.Ada_Name & "_Cache.Cache_Record")));
            if Base.Name = "kit_root_record" then
               Sequence.Append
                 (New_Assignment_Statement
                    (Local_Base (Base)
                     & ".Db.Top_Record",
                     Object ("R_" & Table.Ada_Name)));
            end if;
         end Allocate_Context;

         -----------------
         -- Copy_Record --
         -----------------

         procedure Copy_Record
           (Base : Kit.Schema.Tables.Table_Type)
         is
         begin
            Sequence.Append
              (New_Assignment_Statement
                 ("Result" & Base.Base_Component_Name,
                  Object (Local_Base (Base) & ".Db")));
         end Copy_Record;

         ---------------------
         -- Database_Insert --
         ---------------------

         procedure Database_Insert
           (Base : Kit.Schema.Tables.Table_Type)
         is
            use Syn.Expressions;

            Base_Access : constant String := Local_Base (Base);
            Index_Field : constant String :=
              (if Base.Ada_Name = Table.Ada_Name
               then "Result.M_Index"
               else Local_Base (Table) & ".Db" & Base.Base_Index_Name);

            procedure Set_Base_Index
              (Meta_Base : Kit.Schema.Tables.Table_Type);

            --------------------
            -- Set_Base_Index --
            --------------------

            procedure Set_Base_Index
              (Meta_Base : Kit.Schema.Tables.Table_Type)
            is
               Target_Component : constant String :=
                 Base_Access & ".Db" & Meta_Base.Base_Index_Name;
               Source_Component : constant String :=
                 Local_Base (Table) & ".Db" & Meta_Base.Base_Index_Name;
            begin
               Sequence.Append
                 (New_Assignment_Statement
                    (Target_Component, Object (Source_Component)));
            end Set_Base_Index;

         begin
            Sequence.Append (Base.Ada_Name & "_Impl.File_Mutex.Lock");

            Sequence.Append
              (New_Assignment_Statement
                 (Index_Field,
                  New_Function_Call_Expression
                    (Base.Reference_Type_Name,
                     New_Function_Call_Expression
                       ("Marlowe_Keys.Handle.Insert_Record",
                        Literal (Integer (Base.Reference_Index))))));
            if Base.Ada_Name /= Table.Ada_Name then
               Base.Iterate (Set_Base_Index'Access, Inclusive => False);
            end if;

            Sequence.Append
              (New_Procedure_Call_Statement
                 (Base_Access & ".Initialise",
                  Literal (Integer (Base.Reference_Index)),
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Object (Index_Field))));

            Sequence.Append
              (New_Procedure_Call_Statement
                 ("Kit.Cache.Insert",
                  New_Function_Call_Expression
                    ("Kit.Cache.Cache_Entry", Base_Access)));

            Sequence.Append
              (New_Procedure_Call_Statement
                 (Base.Ada_Name & "_Impl.Write",
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Object (Index_Field)),
                  Object (Base_Access & ".Db")));

            Perform_X_Lock
              (Sequence       => Sequence,
               Table_Name     => Base.Ada_Name,
               Object_Name    => Base_Access,
               Request_Object =>
                 "Result.T" & Base.Index_Image & "_Request",
               Index_Object   => Index_Field);

            Sequence.Append (Base.Ada_Name & "_Impl.File_Mutex.Unlock");
         end Database_Insert;

         ----------------------
         -- Initialise_Field --
         ----------------------

         procedure Initialise_Field
           (Base     : Kit.Schema.Tables.Table_Type;
            Field    : Kit.Schema.Fields.Field_Type)
         is
            pragma Unreferenced (Base);
         begin
            if Field.Created then
               Create_Ref_Block.Append
                 (New_Procedure_Call_Statement
                    ("Result.Set_" & Field.Ada_Name,
                     Object (Field.Ada_Name)));
               Got_Field := True;
            end if;
         end Initialise_Field;

         ---------------
         -- Set_Field --
         ---------------

         procedure Set_Field (Field_Name : String;
                              Value      : String)
         is
         begin
            Sequence.Append
              (Syn.Statements.New_Assignment_Statement
                 ("Result." & Field_Name,
                  Syn.Object (Value)));
         end Set_Field;

      begin

         Set_Field ("Finished", "False");
         Set_Field ("Created", "True");
         Set_Field ("Deleted", "False");
         Set_Field ("Scanning", "False");
         Set_Field ("Read_Only", "False");
         Set_Field ("Using_Key", "False");

         Set_Field ("M_Index", "0");

         Sequence.Append
           (New_Procedure_Call_Statement ("Memory_Mutex.Lock"));

         Table.Iterate (Allocate_Context'Access,
                        Inclusive => True,
                        Table_First => False);

         Sequence.Append
           (New_Procedure_Call_Statement ("Memory_Mutex.Unlock"));

         Table.Iterate (Database_Insert'Access,
                        Inclusive => True,
                        Table_First => False);

         Table.Iterate (Copy_Record'Access,
                        Inclusive => True,
                        Table_First => False);

         Sequence.Append ("Result.Read_Only := False");

         declare
            Block : Syn.Blocks.Block_Type;

            procedure Declare_Access
              (Base : Kit.Schema.Tables.Table_Type);

            --------------------
            -- Declare_Access --
            --------------------

            procedure Declare_Access
              (Base : Kit.Schema.Tables.Table_Type)
            is
            begin
               Block.Add_Declaration
                 (Declarations.New_Object_Declaration
                    (Name        => Local_Base (Base),
                     Object_Type =>
                       Named_Subtype
                         (Base.Ada_Name & "_Cache.Cache_Access")));
            end Declare_Access;

         begin

            Table.Iterate (Declare_Access'Access,
                           Inclusive   => True,
                           Table_First => False);

            Block.Append
              (New_Return_Statement
                 ("Result", Update_Type, Sequence));
            Table_Package.Append
              (New_Function ("Create", Table.Update_Type_Name,
               Block));
         end;

         Create_Ref_Block.Add_Declaration
           (New_Object_Declaration
              ("Result",
               Table.Update_Type_Name,
               Object ("Create")));

         Table.Iterate_All (Initialise_Field'Access);

         if Got_Field then
            declare
               Create_Ref_Proc : Subprogram_Declaration'Class :=
                                   New_Procedure
                                     (Name        => "Create",
                                      Block       => Create_Ref_Block);
            begin
               Add_Create_Arguments (Create_Ref_Proc);
               Table_Package.Append (Create_Ref_Proc);
            end;
         end if;

         Create_Ref_Block.Append
           (New_Return_Statement
              (Object ("Result.Get_" & Table.Ada_Name & "_Reference")));

         declare
            Create_Ref_Fn : Subprogram_Declaration'Class :=
                              New_Function
                                (Name        => "Create",
                                 Result_Type => Table.Reference_Type_Name,
                                 Block       => Create_Ref_Block);
         begin
            Add_Create_Arguments (Create_Ref_Fn);
            Table_Package.Append (Create_Ref_Fn);
         end;

         Table_Package.Append (New_Separator);

      end Add_Create_Function;

      ---------------
      -- Add_Fetch --
      ---------------

      procedure Add_Fetch (Base  : Kit.Schema.Tables.Table_Type;
                           Field : Kit.Schema.Fields.Field_Type)
      is
      begin
         if not Field.Readable then
            return;
         end if;

         if Base.Name = Table.Name then
            declare
               Fetch : constant Subprogram_Declaration'Class :=
                         New_Abstract_Function
                           (Field.Safe_Ada_Name ("Get_"),
                            New_Formal_Argument ("Item",
                              Named_Subtype
                                (Table.Ada_Name & "_Interface")),
                            Named_Subtype
                              (Field.Get_Field_Type.Return_Subtype));
            begin
               Table_Package.Append (Fetch);
               Table_Package.Append (New_Separator);
            end;
         end if;

         declare
            use Syn.Statements;
            Block  : Syn.Blocks.Block_Type;
         begin

            Block.Add_Declaration
              (Syn.Declarations.Renaming_Declaration
                 ("Result",
                  Field.Get_Field_Type.Unconstrained_Record_Subtype,
                  Object
                    (Table.Base_Field_Name ("Item", Base, Field))));

            Block.Add_Statement
              (New_Return_Statement
                 (Field.Get_Field_Type.Return_Value ("Result")));

            declare
               Fetch  : Subprogram_Declaration'Class :=
                          New_Function
                            (Field.Ada_Name,
                             Field.Get_Field_Type.Return_Subtype,
                             Block);
            begin
               Fetch.Add_Formal_Argument
                 ("Item",
                  Table.Ada_Name & "_Implementation");

               Fetch.Set_Overriding;
               Table_Package.Append_To_Body (Fetch);
            end;
         end;

      end Add_Fetch;

      -------------------------
      -- Add_Field_Type_With --
      -------------------------

      procedure Add_Field_Type_With
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
      begin
         if Field.Get_Field_Type.Is_Table_Reference then
            declare
               Table_Name : constant String :=
                              Field.Get_Field_Type.Referenced_Table_Name;
            begin
               if not Withed_Tables.Contains (Table_Name)
                 and then Table_Name /= Table.Ada_Name
               then
                  Table_Package.With_Package
                    (Db.Ada_Name & "." & Table_Name,
                     Body_With => Base.Standard_Name /= Table.Standard_Name);
                  Withed_Tables.Insert (Table_Name);
               end if;
            end;
         elsif Field.Get_Field_Type.Is_External_Type then
            declare
               Type_Package : constant String :=
                                Field.Get_Field_Type
                                  .External_Type_Package_Name;
            begin
               if not Withed_Tables.Contains (Type_Package) then
                  Table_Package.With_Package
                    (Type_Package);
                  Withed_Tables.Insert (Type_Package);
               end if;
            end;
         end if;
      end Add_Field_Type_With;

      ---------------
      -- Add_Store --
      ---------------

      procedure Add_Store (Base  : Kit.Schema.Tables.Table_Type;
                           Field : Kit.Schema.Fields.Field_Type)
      is
      begin
         if Field.Writeable then
            Create_Field_Store_Procedure
              (Db    => Db,
               Table => Table,
               Base  => Base,
               Field => Field,
               Top   => Table_Package);
         end if;
      end Add_Store;

      ---------------------
      -- Add_Update_Base --
      ---------------------

      procedure Add_Update_Base (It : Kit.Schema.Tables.Table_Type) is
      begin
         Table_Update_Interface.Add_Parent
           (Db.Ada_Name & "." & It.Ada_Name & "." &
              It.Update_Interface_Name);
      end Add_Update_Base;

      --------------------------------
      -- Create_Implementation_Type --
      --------------------------------

      procedure Create_Implementation_Type is

         Record_Defn : Syn.Types.Record_Type_Definition;

         procedure Add_Base_Component
           (It : Kit.Schema.Tables.Table_Type);

         ------------------------
         -- Add_Base_Component --
         ------------------------

         procedure Add_Base_Component
           (It : Kit.Schema.Tables.Table_Type)
         is
            Name : constant String := "T" & It.Index_Image;
         begin
            Record_Defn.Add_Component
              (Name & "_Data",
               It.Ada_Name & "_Impl." & It.Ada_Name & "_Database_Record");
         end Add_Base_Component;

      begin
         Record_Defn.Set_Limited;
         Record_Defn.Add_Parent ("Ada.Finalization.Limited_Controlled");
         Record_Defn.Add_Parent (Table.Interface_Name);
         Record_Defn.Add_Component ("Finished", "Boolean");
         Record_Defn.Add_Component ("Forward", "Boolean");
         Record_Defn.Add_Component ("Read_Only", "Boolean");
         Record_Defn.Add_Component ("Created", "Boolean");
         Record_Defn.Add_Component ("Deleted", "Boolean");
         Record_Defn.Add_Component ("Scanning", "Boolean");
         Record_Defn.Add_Component ("Has_Finish", "Boolean");
         Record_Defn.Add_Component ("Start_Closed", "Boolean");
         Record_Defn.Add_Component ("Finish_Closed", "Boolean");
         Record_Defn.Add_Component ("Using_Key", "Boolean");
         Record_Defn.Add_Component ("Using_Key_Value", "Boolean");
         Record_Defn.Add_Component ("Key_Ref",
                                    "Marlowe.Data_Stores.Key_Reference");
         Record_Defn.Add_Component
           ("M_Index", Table.Reference_Type_Name);

         Table.Iterate (Add_Base_Component'Access,
                        Inclusive => True, Table_First => False);

         Table_Package.Append_To_Body
           (New_Full_Type_Declaration
              (Implementation_Type, Record_Defn));

         --  Table_Package.Append_To_Body
         --    (New_Full_Type_Declaration
         --       ("Implementation_Access",
         --        New_Access_Type
         --          (Access_To  => Implementation_Type,
         --           Access_All => True)));

         Create_Search_Procedures (Db, Table, Table_Package);

      end Create_Implementation_Type;

      --------------------
      -- Create_Key_Get --
      --------------------

      procedure Create_Key_Get
        (Base  : Kit.Schema.Tables.Table_Type;
         Key   : Kit.Schema.Keys.Key_Type)
      is
      begin
         if Key.Field_Count = 1
           and then Key.Field (1).Base_Reference
         then
            Public_Get.Create_Unique_Get_Function
              (Table         => Table,
               Key_Table     => Base,
               Table_Package => Table_Package,
               Key_Name      => Key.Standard_Name);
         else
            for Use_Key_Value in Boolean loop
               if Use_Key_Value then
                  if Key.Unique then
                     Public_Get.Create_Unique_Get_Function
                       (Table         => Table,
                        Key_Table     => Base,
                        Table_Package => Table_Package,
                        Key_Name      => Key.Standard_Name);
                  else
                     Public_Get.Create_First_Last_Functions
                       (Table         => Table,
                        Key_Table     => Base,
                        Table_Package => Table_Package,
                        Key_Name      => Key.Standard_Name);
                  end if;
               end if;

               Public_Get.Create_Selection_Function
                 (Db            => Db,
                  Table         => Table,
                  Key_Table     => Base,
                  Table_Package => Table_Package,
                  Key_Name      => Key.Standard_Name,
                  Key_Value     => Use_Key_Value,
                  Bounds        => False);
            end loop;

            for I in 1 .. Key.Field_Count loop
               if not Key.Field (I).Get_Field_Type.Is_Table_Reference then
                  Public_Get.Create_Selection_Function
                    (Db            => Db,
                     Table         => Table,
                     Key_Table     => Base,
                     Table_Package => Table_Package,
                     Key_Name      => Key.Standard_Name,
                     Key_Value     => True,
                     Bounds        => True,
                     Bounded_Index => I);
               end if;
            end loop;

            if Base.Ada_Name = Table.Ada_Name
            --  and then Key.Ada_Name = Table.Ada_Name
              and then Key.Unique
            then
               --  a unique key with the same name as its table is
               --  understood to be a default key
               Public_Get.Create_Default_Key_Functions
                 (Table, Table_Package, Key);

            end if;
         end if;

      end Create_Key_Get;

      --------------------------
      -- Create_Reference_Get --
      --------------------------

      procedure Create_Reference_Get
        (Base : Kit.Schema.Tables.Table_Type)
      is
      begin

         if Base.Ada_Name = Table.Ada_Name then
            declare
               Abstract_Get : constant Subprogram_Declaration'Class :=
                                New_Abstract_Function
                                  ("Get_" & Base.Ada_Name & "_Reference",
                                   New_Formal_Argument ("Item",
                                     Named_Subtype
                                       (Table.Ada_Name & "_Interface")),
                                   Named_Subtype
                                     (Base.Ada_Name & "_Reference"));
            begin
               Table_Package.Append (Abstract_Get);
               Table_Package.Append (New_Separator);
            end;
         end if;

         declare
            Index_Expression : constant String :=
                                 Table.Database_Index_Component
                                   ("Item", Base);
            Get              : Subprogram_Declaration'Class :=
                                 New_Function
                                   ("Get_" & Base.Ada_Name & "_Reference",
                                    Base.Ada_Name & "_Reference",
                                    Object (Index_Expression));
         begin

            Get.Add_Formal_Argument
              ("Item",
               Table.Ada_Name & "_Implementation");

            Get.Set_Overriding;
            Table_Package.Append_To_Body (Get);
         end;

      end Create_Reference_Get;

      ---------------------------
      -- Create_Selection_Type --
      ---------------------------

      procedure Create_Selection_Type is

         Selection          : Syn.Types.Record_Type_Definition;
         Cursor             : Syn.Types.Record_Type_Definition;
         Constant_Reference : Syn.Types.Record_Type_Definition;
         Variable_Reference : Syn.Types.Record_Type_Definition;
         Iterator_Package   : Syn.Declarations.Package_Type :=
                                Syn.Declarations.New_Package_Type
                                  ("Selection_Iterator_Interfaces");

         List_Package   : Syn.Declarations.Package_Type :=
           Syn.Declarations.New_Package_Type ("List_Of_References");
      begin

         List_Package.Set_Generic_Instantiation
           ("Ada.Containers.Doubly_Linked_Lists");
         List_Package.Add_Generic_Actual_Argument
           (Table.Reference_Type_Name);
         List_Package.Set_Private_Spec;
         Table_Package.Append (List_Package);

         Cursor.Add_Component
           ("Current", "List_Of_References.Cursor");

         Table_Package.Append
           (New_Private_Type_Declaration
              ("Cursor", Cursor));

         declare
            use Syn.Expressions;
            use Syn.Statements;
            Has_Element_Block : Syn.Blocks.Block_Type;
         begin
--        return List_Of_Marks.Has_Element (Item.Current_Mark)
--          and then Marlowe.Btree_Handles.Valid
--            (List_Of_Marks.Element (Item.Current_Mark).all);

            Has_Element_Block.Add_Statement
              (New_Return_Statement
                 (New_Function_Call_Expression
                      ("List_Of_References.Has_Element",
                       Object ("Item.Current"))));

            Table_Package.Append
              (New_Function
                 (Name => "Has_Element",
                  Argument =>
                    New_Formal_Argument ("Item", Named_Subtype ("Cursor")),
                  Result_Type => "Boolean",
                  Block    => Has_Element_Block));
         end;

         Constant_Reference.Add_Variant
           ("Element",
            "not null access constant " & Table.Reference_Type_Name);

         declare
            Ref_Type : Syn.Declaration'Class :=
                         New_Private_Type_Declaration
                           ("Constant_Reference_Type", Constant_Reference);
         begin
            Ref_Type.Add_Aspect ("Implicit_Dereference",
                                 Object ("Element"));
            Table_Package.Append (Ref_Type);
         end;

         Variable_Reference.Add_Variant
           ("Element",
            "not null access " & Table.Type_Name);
         declare
            Ref_Type : Syn.Declaration'Class :=
                         New_Private_Type_Declaration
                           ("Reference_Type", Variable_Reference);
         begin
            Ref_Type.Add_Aspect ("Implicit_Dereference",
                                 Object ("Element"));
            Table_Package.Append (Ref_Type);
         end;

         Selection.Set_Tagged;
         Selection.Add_Component
           ("Elements",
            "List_Of_References.List");
         Iterator_Package.Set_Generic_Instantiation
           ("Ada.Iterator_Interfaces");
         Iterator_Package.Add_Generic_Actual_Argument ("Cursor");
         Iterator_Package.Add_Generic_Actual_Argument ("Has_Element");
         Table_Package.Append (Iterator_Package);

         declare
            Selection_Type : Type_Declaration :=
                               New_Private_Type_Declaration
                                 ("Selection", Selection);
         begin
            Selection_Type.Add_Aspect ("Constant_Indexing",
                                       "Constant_Reference");
            --  Selection_Type.Add_Aspect ("Variable_Indexing",
            --                             "Variable_Reference");
            Selection_Type.Add_Aspect ("Default_Iterator",
                                       "Iterate");
            Selection_Type.Add_Aspect ("Iterator_Element",
                                       Table.Reference_Type_Name);

            Table_Package.Append (Selection_Type);
         end;

         declare
            use Syn.Statements;
            Iterate_Block : Syn.Blocks.Block_Type;
            Return_Sequence : Sequence_Of_Statements;
         begin
            Return_Sequence.Append
              (New_Assignment_Statement
                 ("Result.Container",
                  Object ("Container'Unrestricted_Access")));
            Iterate_Block.Add_Statement
              (New_Return_Statement
                 (Return_Variable   => "Result",
                  Variable_Type     => "Iterator",
                  Return_Statements => Return_Sequence));
            Table_Package.Append
              (New_Function
                 (Name => "Iterate",
                  Argument =>
                    New_In_Argument
                      ("Container",
                       Named_Subtype ("Selection")),
                  Result_Type =>
                    "Selection_Iterator_Interfaces.Reversible_Iterator'Class",
                  Block       => Iterate_Block));
         end;

         declare
            Ref_Block : Syn.Blocks.Block_Type;
         begin
            --  Ref_Block.Add_Declaration
            --    (Syn.Declarations.New_Pragma
            --       ("Unreferenced", "Container"));

            Ref_Block.Add_Statement
              (Syn.Statements.New_Return_Statement
                 (Result =>
                      Object
                    ("(Element => "
                     & "Container.Elements.Constant_Reference "
                     & "(Position.Current).Element)")));
            declare
               Fn_Name   : constant String :=
                 "Constant_Reference";
               Ref_Fn    : Subprogram_Declaration'Class :=
                 New_Function
                   (Fn_Name,
                    Named_Subtype
                      ("Constant_Reference_Type"),
                    Ref_Block);
               Container : Formal_Argument'Class :=
                 New_Formal_Argument
                   ("Container",
                    In_Argument,
                    Named_Subtype
                      ("Selection"));
               Position  : constant Formal_Argument'Class :=
                 New_In_Argument
                   ("Position",
                    Named_Subtype ("Cursor"));
               Inline    : Declaration'Class :=
                 New_Pragma ("Inline", Fn_Name);
            begin
               Container.Set_Aliased;
               Ref_Fn.Add_Formal_Argument (Container);
               Ref_Fn.Add_Formal_Argument (Position);
               Table_Package.Append (Ref_Fn);

               Inline.Set_Private_Spec;
               Table_Package.Append (Inline);
            end;
         end;

         declare
            use Syn.Expressions;
            use Syn.Statements;
            Is_Empty : constant Subprogram_Declaration'Class :=
                         New_Function
                           (Name        => "Is_Empty",
                            Argument    =>
                              New_In_Argument
                                (Name          => "Container",
                                 Argument_Type =>
                                   Named_Subtype ("Selection")),
                            Result_Type => "Boolean",
                            Block       =>
                              Syn.Blocks.Create_Block
                                (New_Return_Statement
                                     (New_Function_Call_Expression
                                          ("Has_Element",
                                           Object ("Container.First"))
                                     )
                                )
                           );
         begin
            Table_Package.Append (Is_Empty);
         end;

         declare
            use Syn.Statements;
            Element : constant Subprogram_Declaration'Class :=
                         New_Function
                           (Name        => "Element",
                            Argument    =>
                              New_In_Argument
                                (Name          => "Item",
                                 Argument_Type =>
                                   Named_Subtype ("Cursor")),
                            Result_Type => Table.Reference_Type_Name,
                            Block       =>
                              Syn.Blocks.Create_Block
                                (New_Return_Statement
                                     (Object
                                          ("List_Of_References.Element "
                                           & "(Item.Current)")
                                     )
                                )
                           );
         begin
            Table_Package.Append (Element);
         end;

         declare
            use Syn.Expressions;
            use Syn.Statements;
            Block : Syn.Blocks.Block_Type;
         begin
            Block.Add_Declaration
              (New_Object_Declaration
                 ("Result", "Natural", Literal (0)));
            Block.Add_Declaration
              (New_Object_Declaration
                 ("It", "Cursor", Object ("Container.First")));

            declare
               While_Sequence : Sequence_Of_Statements;
            begin
               While_Sequence.Append
                 (New_Assignment_Statement
                    ("Result",
                     Operator ("+", Object ("Result"), Literal (1))));
               While_Sequence.Append
                 (New_Procedure_Call_Statement
                    ("Next", Object ("It")));
               Block.Add_Statement
                 (Syn.Statements.While_Statement
                    (Condition  =>
                       New_Function_Call_Expression
                         ("Has_Element", "It"),
                     While_Body => While_Sequence));
            end;

            Block.Add_Statement
              (New_Return_Statement
                 (Object ("Result")));

            Table_Package.Append
              (New_Function
                 (Name        => "Length",
                  Argument    =>
                    New_In_Argument
                      (Name          => "Container",
                       Argument_Type =>
                         Named_Subtype ("Selection")),
                  Result_Type => "Natural",
                  Block       => Block));
         end;

      end Create_Selection_Type;

      ------------------------
      -- Create_Update_Type --
      ------------------------

      procedure Create_Update_Type is
         Update_Defn : Syn.Types.Record_Type_Definition;
      begin
         Update_Defn.Set_Limited;
         Update_Defn.Add_Parent (Implementation_Type);
         Update_Defn.Add_Parent (Table.Update_Interface_Name);
         Update_Defn.Add_Parent ("Record_Update_Interface");
         Table_Package.Append_To_Body
           (New_Full_Type_Declaration
              (Table.Update_Implementation_Name, Update_Defn));

      end Create_Update_Type;

   begin

      Table_Package.With_Package ("Ada.Containers.Doubly_Linked_Lists",
                                  Private_With => True);

      Table_Package.With_Package ("Ada.Finalization", Body_With => True);

      Table_Package.With_Package ("System.Storage_Elements",
                                  Body_With => True);

      Table_Package.With_Package ("Marlowe.Data_Stores",
                                  Body_With => True);

      Table_Package.With_Package ("Ada.Iterator_Interfaces");
      if Table.Has_Key_Field then
         Table_Package.With_Package ("Marlowe.Key_Storage",
                                     Body_With => True);
      end if;

      if not Table.Is_Abstract then
         Table_Package.With_Package
           ("Kit.Cache",
            Body_With => True);
      end if;

      Table_Package.With_Package
        ("Kit.Notifier", Body_With => True);

      if Table.Has_String_Type then
         Table_Package.With_Package ("Kit.Strings",
                                     Body_With => True);
      end if;

      if Table.Has_Text_Type then
         Table_Package.With_Package ("Kit.Text",
                                     Body_With => True);
      end if;

      Table_Package.With_Package (Db.Ada_Name & ".Kit_Deferred_Keys",
                                  Body_With => True);
      Table_Package.With_Package (Db.Ada_Name & ".Marlowe_Keys",
                                  Body_With => True);

      if Option_Deadlock_Detection then
         Table_Package.With_Package
           (Db.Ada_Name & ".Kit_Locking",
            Body_With => True);
         Table_Package.With_Package
           (Db.Ada_Name & ".Table_Names",
            Body_With => True);
      end if;

      Table.Iterate (Add_Base_With'Access, Inclusive => False);
      Table.Iterate_All (Add_Field_Type_With'Access,
                         Table_First => True);

      Table_Interface.Set_Limited;
      Table_Interface.Add_Parent ("Record_Interface");
      Table_Interface.Add_Parent ("Search_Interface");

      Table.Iterate (Add_Base'Access, Inclusive => False);

      Table_Package.With_Package
        (Db.Ada_Name & "." & Database_Package,
         Body_With => True);

      Table_Package.With_Package
        (Db.Ada_Name & "." & Cache_Package,
         Body_With => True);

      if Option_Debug then
         Table_Package.With_Package
           ("Ada.Text_IO",
            Body_With => True);
      end if;

      Create_Field_Constants (Table, Table_Package);

      Table_Package.Append
        (New_Full_Type_Declaration
           (Table.Interface_Name,
            Table_Interface));
      Table_Package.Append
        (New_Separator);

      Table_Package.Append
        (New_Subtype_Declaration
           (Table.Type_Name,
            Syn.Class_Wide_Subtype
              (Table.Interface_Name)));

      Create_Implementation_Type;

      Table_Package.Append
        (New_Separator);

      Create_Overrides (Db, Table, Table_Package);

      Table.Iterate (Create_Reference_Get'Access,
                     Inclusive   => True,
                     Table_First => False);

      Table.Iterate_All (Add_Fetch'Access);

      Table_Update_Interface.Set_Limited;
      Table_Update_Interface.Add_Parent (Table.Interface_Name);
      Table.Iterate (Add_Update_Base'Access, Inclusive => False);

      Table_Package.Append
        (New_Full_Type_Declaration
           (Table.Update_Interface_Name,
            Table_Update_Interface));
      Table_Package.Append
        (New_Separator);

      Table_Package.Append
        (New_Subtype_Declaration
           (Table.Update_Type_Name,
            Syn.Class_Wide_Subtype
              (Table.Update_Interface_Name)));

      if Table.Ada_Name = "Kit_Root_Record" then
         Table_Package.Append
           (New_Abstract_Procedure
              (Name     => "Delete",
               Argument =>
                 New_Inout_Argument
                   ("Item",
                    Syn.Named_Subtype (Table.Update_Interface_Name))));
      end if;

      if not Table.Is_Abstract then
         Add_Create_Function;
      end if;

      Create_Selection_Type;

      if False then
         Public_Get.Create_Get_From_Index (Table, Table_Package);
      end if;

      Create_Identity_Function (Table, Table_Package);
      Create_Generic_Get (Table, Table_Package);

      Table.Scan_Keys (Create_Key_Get'Access,
                       Include_Base_Keys => True);

--        if Table.Has_Key_Field then
--           Public_Get.Create_Generic_Get_Function
--             (Db, Table, Table_Package, Key_Value => False);
--           Public_Get.Create_Generic_Get_Function
--             (Db, Table, Table_Package, Key_Value => True);
--        end if;

      Public_Get.Create_Reference_Get_Function
        (Db, Table, Table_Package);

      Public_Get.Create_Iterator (Table, Table_Package);

      Create_Notification_Handles (Db, Table, Table_Package);

      Create_Update_Type;

      Create_Control_Procedures (Db, Table, Table_Package);

      Create_Generic_Set (Table, Table_Package);

      Table.Iterate_All (Add_Store'Access);

      --  if Table.Has_Writable_Field then
      --     Kit.Generate.Updates.Generate_Update_Subprograms
      --       (Table, Table_Package);
      --  end if;

      return Table_Package;
   end Generate_Public_Interface;

   --------------------
   -- Perform_X_Lock --
   --------------------

   procedure Perform_X_Lock
     (Sequence          : in out Syn.Statement_Sequencer'Class;
      Table_Name        : String;
      Object_Name       : String;
      Request_Object    : String;
      Index_Object      : String)
   is
      use Syn;
      use Syn.Statements;
      use Syn.Expressions;
   begin
      if Option_Deadlock_Detection then
         Sequence.Append
           (New_Assignment_Statement
              (Request_Object,
               New_Function_Call_Expression
                 ("Kit_Locking.Request_Lock",
                  Object ("Table_Names." & Table_Name),
                  New_Function_Call_Expression
                    ("Marlowe.Database_Index",
                     Object (Index_Object)),
                  Current_Line)));
      end if;
      Sequence.Append
        (New_Procedure_Call_Statement (Object_Name & ".X_Lock"));
      if Option_Deadlock_Detection then
         Sequence.Append
           (New_Procedure_Call_Statement
              (Procedure_Name => "Kit_Locking.Got_Lock",
               Argument       =>
                 Object (Request_Object)));
      end if;
   end Perform_X_Lock;

end Kit.Generate.Public_Interface;
