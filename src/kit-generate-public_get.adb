with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;

with Kit.Generate.Fetch;
with Kit.Names;

with Kit.Schema.Fields;
with Kit.Schema.Types;

package body Kit.Generate.Public_Get is

   type Non_Iterator_Fetch_Type is (Unique_Get, First, Last);

   procedure Create_Non_Iterator_Fetch
     (Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String;
      Fetch_Type    : Non_Iterator_Fetch_Type);

   procedure Create_Iterator_Start_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Container     : Boolean;
      First         : Boolean);

   procedure Create_Iterator_Next_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      With_Iterator : Boolean;
      Inline        : Boolean;
      Next          : Boolean);

   procedure Check_Deferred_Keys
     (Seq       : in out Syn.Statement_Sequencer'Class;
      Key_Table : Kit.Schema.Tables.Table_Type);

   --  Create a call to Kit_Deferred_Keys.Check_Keys for
   --  the index of each table which is a base of Key_Table (inclusive).
   --  Add each call to Seq

   -------------------------
   -- Check_Deferred_Keys --
   -------------------------

   procedure Check_Deferred_Keys
     (Seq       : in out Syn.Statement_Sequencer'Class;
      Key_Table : Kit.Schema.Tables.Table_Type)
   is
      procedure Check_Keys (Base : Kit.Schema.Tables.Table_Type);

      ----------------
      -- Check_Keys --
      ----------------

      procedure Check_Keys (Base : Kit.Schema.Tables.Table_Type) is
      begin
         Seq.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Kit_Deferred_Keys.Check_Keys",
                  Syn.Literal
                 (Natural (Base.Reference_Index))));
      end Check_Keys;

   begin
      Key_Table.Iterate (Check_Keys'Access, Inclusive => True);
   end Check_Deferred_Keys;

   ----------------------------------
   -- Create_Default_Key_Functions --
   ----------------------------------

   procedure Create_Default_Key_Functions
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key           : Kit.Schema.Keys.Key_Type)
   is
      use Syn.Declarations;
      Ask   : Syn.Expressions.Function_Call_Expression :=
                Syn.Expressions.New_Function_Call_Expression
                  ("Get_By_" & Key.Ada_Name);
      Block : Syn.Blocks.Block_Type;
   begin

      for I in 1 .. Key.Field_Count loop
         declare
            Field : Kit.Schema.Fields.Field_Type renames
                      Key.Field (I);
         begin
            Ask.Add_Actual_Argument
              (Syn.Object (Field.Ada_Name));
         end;
      end loop;

      Block.Add_Statement
        (Syn.Statements.New_Return_Statement
           (Syn.Expressions.Operator
                ("/=", Ask,
                 Syn.Object ("Null_" & Table.Ada_Name & "_Reference"))));

      declare
         Fn : Subprogram_Declaration'Class :=
                New_Function
                  ("Is_" & Key.Ada_Name,
                   "Boolean",
                   Block);
      begin
         for I in 1 .. Key.Field_Count loop
            declare
               Field : Kit.Schema.Fields.Field_Type renames
                         Key.Field (I);
            begin
               Fn.Add_Formal_Argument
                 (New_Formal_Argument
                    (Field.Ada_Name,
                     Syn.Named_Subtype
                       (Field.Get_Field_Type.Argument_Subtype)));
            end;
         end loop;

         Table_Package.Append (Fn);
      end;

      Table_Package.Append (Syn.Declarations.New_Separator);

   end Create_Default_Key_Functions;

   ---------------------------------
   -- Create_First_Last_Functions --
   ---------------------------------

   procedure Create_First_Last_Functions
     (Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String)
   is
   begin
      Create_Non_Iterator_Fetch
        (Table, Key_Table, Table_Package, Key_Name,
         Fetch_Type => First);
      Create_Non_Iterator_Fetch
        (Table, Key_Table, Table_Package, Key_Name,
         Fetch_Type => Last);
   end Create_First_Last_Functions;

   ---------------------------------
   -- Create_Generic_Get_Function --
   ---------------------------------

   procedure Create_Generic_Get_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Value     : Boolean)
   is
      pragma Unreferenced (Db);

      use Syn;
      use Syn.Declarations;
      Key_Case  : Syn.Statements.Case_Statement_Record'Class :=
                    Syn.Statements.Case_Statement ("Key");
      Block                  : Syn.Blocks.Block_Type;

      Function_Name          : constant String := "Select_By";

      procedure Process_Key (Base  : Kit.Schema.Tables.Table_Type;
                             Key   : Kit.Schema.Keys.Key_Type);

      -----------------
      -- Process_Key --
      -----------------

      procedure Process_Key (Base  : Kit.Schema.Tables.Table_Type;
                             Key   : Kit.Schema.Keys.Key_Type)
      is
         pragma Unreferenced (Base);
         use Syn.Expressions;
         Function_Name : constant String :=
                           (if Key_Value
                            then "Select_By_" & Key.Ada_Name
                            else "Scan_By_" & Key.Ada_Name);
         Call : Function_Call_Expression :=
                           New_Function_Call_Expression
                             (Function_Name);
         Seq  : Syn.Statements.Sequence_Of_Statements;
      begin

         if Key_Value and then Key.Field_Count = 1 then
            Call.Add_Actual_Argument
              (Key.Field (1).Get_Field_Type.Convert_From_String ("Value"));
         end if;

         Seq.Append
           (Syn.Statements.New_Return_Statement
              (Call));
         Key_Case.Add_Case_Option ("K_" & Table.Ada_Name & "_"
                                   & Key.Ada_Name,
                                   Seq);
      end Process_Key;

   begin

      Table.Scan_Keys (Process_Key'Access);

      Block.Append (Key_Case);

      declare
         Fn : Subprogram_Declaration'Class :=
                New_Function
                  (Function_Name, "Selection",
                   Block);
      begin
         Fn.Add_Formal_Argument
           ("Key", Table.Ada_Name & "_Key");

         if Key_Value then
            Fn.Add_Formal_Argument ("Value", "String");
         end if;

         Table_Package.Append (Fn);
      end;

      Table_Package.Append (Syn.Declarations.New_Separator);
   end Create_Generic_Get_Function;

   ---------------------------
   -- Create_Get_From_Index --
   ---------------------------

   procedure Create_Get_From_Index
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      use Syn;
      use Syn.Expressions, Syn.Statements;

      Block : Syn.Blocks.Block_Type;

      procedure Set_Field
        (Seq        : in out Syn.Blocks.Block_Type;
         Field_Name : String;
         Value      : Boolean);

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Seq        : in out Syn.Blocks.Block_Type;
         Field_Name : String;
         Value      : Boolean)
      is
      begin
         Seq.Append
           (New_Assignment_Statement
              ("Element." & Field_Name,
               (if Value then Object ("True") else Object ("False"))));
      end Set_Field;

   begin

      Block.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Lock"));

      Block.Append
        (New_Assignment_Statement
           ("Element.M_Index",
            New_Function_Call_Expression
              (Table.Reference_Type_Name, "Index")));

      declare
         Exists_Sequence : Sequence_Of_Statements;
      begin
         Fetch.Fetch_From_Index (Table       => Table,
                                 Object_Name => "Element",
                                 Update      => False,
                                 Target      => Exists_Sequence);
         Block.Append
           (If_Statement
              (Operator
                   ("/=", Object ("Element.M_Index"),
                    Object
                      ("Null_" & Table.Ada_Name & "_Reference")),
               Exists_Sequence));
      end;

      Set_Field (Block, "Finished", False);
      Set_Field (Block, "Using_Key_Value", False);
      Set_Field (Block, "Scanning", False);

      Block.Append
        (New_Procedure_Call_Statement
           (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));

      declare
         use Syn.Declarations;
         Proc : Subprogram_Declaration'Class :=
                  New_Procedure
                    ("Get", Block);
      begin
         Proc.Add_Formal_Argument
           (New_Formal_Argument
              ("Index",
               Named_Subtype ("Marlowe.Database_Index")));
         Proc.Add_Formal_Argument
           (New_Out_Argument
              ("Element",
               Named_Subtype
                 (Table.Ada_Name & "_Implementation'Class")));
         Table_Package.Append_To_Body (Proc);
      end;

      Table_Package.Append_To_Body (Syn.Declarations.New_Separator);
   end Create_Get_From_Index;

   ---------------------
   -- Create_Iterator --
   ---------------------

   procedure Create_Iterator
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is

      use Syn.Declarations, Syn.Types;
      Iterator_Definition : Record_Type_Definition;
   begin

      Table_Package.Append_To_Body
        (New_Full_Type_Declaration
           ("Selection_Access",
            Syn.New_Access_Type
              ("Selection",
               Access_All => False)));

      Iterator_Definition.Add_Parent
        ("Selection_Iterator_Interfaces.Reversible_Iterator");
      Iterator_Definition.Add_Component
        ("Container", "Selection_Access");
      Table_Package.Append_To_Body
        (New_Full_Type_Declaration
           ("Iterator", Iterator_Definition));

      for Container in Boolean loop
         for First in reverse Boolean loop
            Create_Iterator_Start_Function
              (Table, Table_Package,
               Container => Container,
               First     => First);
         end loop;
      end loop;

      for With_Iterator in Boolean loop
         for Next in reverse Boolean loop
            Create_Iterator_Next_Function (Table, Table_Package,
                                           With_Iterator => With_Iterator,
                                           Inline        => False,
                                           Next          => Next);
         end loop;
      end loop;

      for Next in reverse Boolean loop
         Create_Iterator_Next_Function (Table, Table_Package,
                                        With_Iterator => False,
                                        Inline        => True,
                                        Next          => Next);
      end loop;

   end Create_Iterator;

   -----------------------------------
   -- Create_Iterator_Next_Function --
   -----------------------------------

   procedure Create_Iterator_Next_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      With_Iterator : Boolean;
      Inline        : Boolean;
      Next          : Boolean)
   is
      pragma Unreferenced (Table);
      use Syn;
      use Syn.Declarations;
      use Syn.Statements;
      Next_Block        : Syn.Blocks.Block_Type;
      Call              : constant String :=
        (if Next then "Next" else "Previous");
   begin
      if Inline then
         Next_Block.Add_Statement
           (New_Procedure_Call_Statement
              ("List_Of_References." & Call,
               Object ("Position.Current")));
      else
         Next_Block.Add_Statement
           (New_Return_Statement
              (Object
                   ("(Current => "
                    & "List_Of_References." & Call & " "
                    & "(Position.Current))")));
      end if;

      declare
         Next_Declaration : Subprogram_Declaration'Class :=
                              (if Inline
                               then New_Procedure
                                 ((if Next then "Next" else "Previous"),
                                  Next_Block)
                               else New_Function
                                 ((if Next then "Next" else "Previous"),
                                  "Cursor",
                                  Next_Block));
      begin
         if With_Iterator then
            Next_Declaration.Add_Formal_Argument
              ("Object", "Iterator");
         end if;
         if Inline then
            Next_Declaration.Add_Formal_Argument
              ("Position", Inout_Argument, "Cursor");
         else
            Next_Declaration.Add_Formal_Argument
              ("Position", "Cursor");
         end if;
         if With_Iterator then
            Next_Declaration.Set_Overriding;
            Table_Package.Append_To_Body (Next_Declaration);
         else
            Table_Package.Append (Next_Declaration);
         end if;
      end;
   end Create_Iterator_Next_Function;

   ------------------------------------
   -- Create_Iterator_Start_Function --
   ------------------------------------

   procedure Create_Iterator_Start_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Container     : Boolean;
      First         : Boolean)
   is
      pragma Unreferenced (Table);
      use Syn;
      use Syn.Declarations;
      use Syn.Statements;

      function Function_Name return String;

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean);
      pragma Unreferenced (Set_Field);

      -------------------
      -- Function_Name --
      -------------------

      function Function_Name return String is
      begin
         if First then
            return "First";
         else
            return "Last";
         end if;
      end Function_Name;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean)
      is
      begin
         Seq.Append
           (New_Assignment_Statement
              ("Result." & Field_Name,
               (if Value then Object ("True") else Object ("False"))));
      end Set_Field;

   begin
      declare
         Block                  : Syn.Blocks.Block_Type;
      begin
         if Container then
            Block.Append
              (New_Return_Statement
                 (Object
                      ("(Current => Container.Elements."
                       & Function_Name & ")")));
         else
            Block.Append
              (New_Return_Statement
                 (Object
                      ("(Current => Object.Container.Elements."
                       & Function_Name & ")")));
         end if;

         declare
            Fn : Subprogram_Declaration'Class :=
                   New_Function
                     (Function_Name, "Cursor",
                      Block);
         begin
            if not Container then
               Fn.Set_Overriding;
            end if;

            if Container then
               Fn.Add_Formal_Argument
                 ("Container", "Selection");
            else
               Fn.Add_Formal_Argument
                 ("Object", "Iterator");
            end if;

            if Container then
               Table_Package.Append (Fn);
            else
               Table_Package.Append_To_Body (Fn);
               Table_Package.Append_To_Body
                 (Syn.Declarations.New_Separator);
            end if;
         end;

      end;

   end Create_Iterator_Start_Function;

   -------------------------------
   -- Create_Non_Iterator_Fetch --
   -------------------------------

   procedure Create_Non_Iterator_Fetch
     (Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String;
      Fetch_Type    : Non_Iterator_Fetch_Type)
   is
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions, Syn.Statements;

      Key              : constant Kit.Schema.Keys.Key_Type :=
                           Table.Key (Key_Name);

      procedure Create_Function
        (Reference : Boolean);

      ---------------------
      -- Create_Function --
      ---------------------

      procedure Create_Function
        (Reference : Boolean)
      is

         Block            : Syn.Blocks.Block_Type;

         function Function_Name return String;

         procedure Set_Field
           (Seq        : in out Sequence_Of_Statements;
            Field_Name : String;
            Value      : Boolean);

         -------------------
         -- Function_Name --
         -------------------

         function Function_Name return String is
            Fetch_Part     : constant String :=
                               (case Fetch_Type is
                                   when Unique_Get => "Get_",
                                   when First      => "First_",
                                   when Last       => "Last_");
         begin
            if Key.Base_Reference then
               return Fetch_Part & "From_" & Key.Base_Table_Name;
            else
               return Fetch_Part
                 & "By_"
                 & Key.Ada_Name;
            end if;
         end Function_Name;

         ---------------
         -- Set_Field --
         ---------------

         procedure Set_Field
           (Seq        : in out Sequence_Of_Statements;
            Field_Name : String;
            Value      : Boolean)
         is
         begin
            Seq.Append
              (New_Assignment_Statement
                 ("Result." & Field_Name,
                  (if Value then Object ("True") else Object ("False"))));
         end Set_Field;

      begin

         Block.Add_Declaration
           (Use_Type ("System.Storage_Elements.Storage_Array"));
         if not Reference then
            Block.Add_Declaration
              (Use_Type ("Marlowe.Database_Index"));
         end if;

         Block.Add_Declaration
           (Syn.Declarations.New_Object_Declaration
              ("Db_Index", "Marlowe.Database_Index", Literal (0)));

         Check_Deferred_Keys (Block, Key_Table);

         Block.Add_Statement
           (New_Procedure_Call_Statement
              (Table.Ada_Name & "_Impl.File_Mutex.Shared_Lock"));

         declare
            Mark_Block       : Syn.Blocks.Block_Type;
            Initialiser      : Function_Call_Expression :=
                                 New_Function_Call_Expression
                                   ("Marlowe_Keys.Handle.Search");
         begin
            Initialiser.Add_Actual_Argument
              (Object ("Marlowe_Keys." & Table.Key_Reference_Name (Key)));
            declare
               Key_To_Storage   : constant Expression'Class :=
                                    Table.To_Storage
                                      (Table, Key_Table, "", Key,
                                       With_Index => False);
               Start_Storage    : constant Expression'Class :=
                                    New_Function_Call_Expression
                                      ("Marlowe.Key_Storage.To_Storage_Array",
                                       "Marlowe.Database_Index'First");
               Last_Storage     : constant Expression'Class :=
                                    New_Function_Call_Expression
                                      ("Marlowe.Key_Storage.To_Storage_Array",
                                       "Marlowe.Database_Index'Last");
            begin
               Initialiser.Add_Actual_Argument
                 (Operator
                    (Name  => "&",
                     Left  => Key_To_Storage,
                     Right => Start_Storage));
               Initialiser.Add_Actual_Argument
                 (Operator
                    (Name  => "&",
                     Left  => Key_To_Storage,
                     Right => Last_Storage));
            end;

            Initialiser.Add_Actual_Argument
              (Object ("Marlowe.Closed"));
            Initialiser.Add_Actual_Argument
              (Object ("Marlowe.Closed"));
            case Fetch_Type is
               when Unique_Get | First =>
                  Initialiser.Add_Actual_Argument
                    (Object ("Marlowe.Forward"));
               when Last =>
                  Initialiser.Add_Actual_Argument
                    (Object ("Marlowe.Backward"));
            end case;

            Mark_Block.Add_Declaration
              (New_Constant_Declaration
                 ("M", Data_Store_Cursor_Name,
                  Initialiser));
            Mark_Block.Add_Statement
              (If_Statement
                 (Object ("M.Valid"),
                  New_Assignment_Statement
                    ("Db_Index",
                     New_Function_Call_Expression
                       ("Marlowe.Key_Storage.To_Database_Index",
                        Object ("M.Get_Key")))));
            Block.Add_Statement
              (Declare_Statement (Mark_Block));
         end;

         if Reference then
            Block.Add_Statement
              (New_Procedure_Call_Statement
                 (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));
            Block.Add_Statement
              (New_Return_Statement
                 (New_Function_Call_Expression
                      (Table.Ada_Name & "_Reference",
                       Object ("Db_Index"))));
         else

            declare
               Return_Sequence  : Sequence_Of_Statements;
               Valid_Sequence   : Sequence_Of_Statements;
               Invalid_Sequence : Sequence_Of_Statements;
            begin

               Return_Sequence.Append
                 (New_Assignment_Statement
                    (Target => "Result.M_Index",
                     Value  =>
                       New_Function_Call_Expression
                         (Table.Reference_Type_Name, "Db_Index")));

               Fetch.Fetch_From_Index (Table       => Table,
                                       Object_Name => "Result",
                                       Update      => False,
                                       Target      => Valid_Sequence);

               Set_Field (Valid_Sequence, "Finished", False);
               Set_Field (Valid_Sequence, "Using_Key_Value", False);
               Set_Field (Valid_Sequence, "Scanning", False);

               Set_Field (Invalid_Sequence, "Finished", True);
               Set_Field (Invalid_Sequence, "Using_Key_Value", False);
               Set_Field (Invalid_Sequence, "Scanning", False);

               Return_Sequence.Append
                 (If_Statement
                    (Operator ("/=", Object ("Db_Index"), Literal (0)),
                     Valid_Sequence,
                     Invalid_Sequence));
               Return_Sequence.Append
                 (New_Procedure_Call_Statement
                    (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));

               Block.Add_Statement
                 (New_Return_Statement
                    ("Result", Table.Implementation_Name,
                     Return_Sequence));
            end;
         end if;

         declare
            Result_Type : constant String :=
                            (if Reference
                             then Table.Ada_Name & "_Reference"
                             else Table.Type_Name);
            Fn          : Subprogram_Declaration'Class :=
                            New_Function
                              (Function_Name, Result_Type,
                               Block);
         begin
            for I in 1 .. Key.Field_Count loop
               declare
                  Field : Kit.Schema.Fields.Field_Type
                  renames Key.Field (I);
               begin
                  Fn.Add_Formal_Argument
                    (New_Formal_Argument
                       (Field.Ada_Name,
                        Named_Subtype
                          (Field.Get_Field_Type.Argument_Subtype)));
               end;
            end loop;
            Table_Package.Append (Fn);
         end;
      end Create_Function;

   begin
      Create_Function (True);

      --  for Reference in Boolean loop
      --     Create_Function (Reference);
      --  end loop;
      Table_Package.Append (Syn.Declarations.New_Separator);
   end Create_Non_Iterator_Fetch;

   -----------------------------------
   -- Create_Reference_Get_Function --
   -----------------------------------

   procedure Create_Reference_Get_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Db);
      use Syn;
      use Syn.Expressions, Syn.Statements;

      Empty_Sequence   : Sequence_Of_Statements;
      Return_Sequence  : Sequence_Of_Statements;

      function Function_Name
        (For_Update : Boolean)
         return String;

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean);

      -------------------
      -- Function_Name --
      -------------------

      function Function_Name
        (For_Update : Boolean)
         return String
      is
      begin
         if For_Update then
            return "Get_Update";
         else
            return "Get";
         end if;
      end Function_Name;

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Seq        : in out Sequence_Of_Statements;
         Field_Name : String;
         Value      : Boolean)
      is
      begin
         Seq.Append
           (New_Assignment_Statement
              ("Result." & Field_Name,
               (if Value then Object ("True") else Object ("False"))));
      end Set_Field;

   begin

      for Update in Boolean loop
         Return_Sequence := Empty_Sequence;
         Return_Sequence.Append
           (New_Procedure_Call_Statement
              (Table.Ada_Name & "_Impl.File_Mutex.Shared_Lock"));

         Return_Sequence.Append
           (New_Assignment_Statement
              ("Result.M_Index", Object ("Ref")));

         declare
            Exists_Sequence : Sequence_Of_Statements;
         begin
            Fetch.Fetch_From_Index (Table       => Table,
                                    Object_Name => "Result",
                                    Update      => Update,
                                    Target      => Exists_Sequence);
            Return_Sequence.Append
              (If_Statement
                 (Operator
                      ("/=", Object ("Result.M_Index"),
                       Object
                         ("Null_" & Table.Ada_Name & "_Reference")),
                  Exists_Sequence));
         end;

         Set_Field (Return_Sequence, "Finished", False);
         Set_Field (Return_Sequence, "Using_Key_Value", False);
         Set_Field (Return_Sequence, "Scanning", False);

         if Update then
            Set_Field (Return_Sequence, "Read_Only", Value => False);
         end if;

         Return_Sequence.Append
           (New_Procedure_Call_Statement
              (Table.Ada_Name & "_Impl.File_Mutex.Shared_Unlock"));

         declare
            use Syn.Declarations;
            Result_Name : constant String :=
                            (if Update
                             then Table.Update_Implementation_Name
                             else Table.Implementation_Name);
            Return_Name : constant String :=
                            (if Update
                             then Table.Update_Type_Name
                             else Table.Type_Name);
            Block       : Syn.Blocks.Block_Type;
         begin
            Block.Append
              (Syn.Statements.New_Return_Statement
                 ("Result", Result_Name, Return_Sequence));

            declare
               Fn : Subprogram_Declaration'Class :=
                      New_Function
                        (Function_Name (Update), Return_Name, Block);
            begin
               Fn.Add_Formal_Argument
                 (New_Formal_Argument
                    ("Ref",
                     Named_Subtype
                       (Table.Ada_Name & "_Reference")));
               Table_Package.Append (Fn);
            end;

         end;
      end loop;

      Table_Package.Append (Syn.Declarations.New_Separator);
   end Create_Reference_Get_Function;

   -------------------------------
   -- Create_Selection_Function --
   -------------------------------

   procedure Create_Selection_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String;
      Key_Value     : Boolean;
      Bounds        : Boolean;
      Bounded_Index : Natural   := 0)
   is

      pragma Unreferenced (Db);
      use Syn;
      use Syn.Expressions, Syn.Statements;

      Scanning : constant Boolean :=
        not Bounds and then not Key_Value
        with Unreferenced;

      Function_Body    : Blocks.Block_Type;
      Return_Sequence  : Sequence_Of_Statements;
      Key              : constant Kit.Schema.Keys.Key_Type :=
                           Table.Key (Key_Name);
      Field_Count      : constant Positive :=
                           (if not Bounds
                            or else Bounded_Index = 0
                            then Key.Field_Count
                            else Bounded_Index);

      function Function_Name return String;

      function To_Storage
        (First : Boolean)
         return Expression'Class;

      -------------------
      -- Function_Name --
      -------------------

      function Function_Name return String is
      begin
         if Bounds then
            if Bounded_Index = 0
              or else Key.Field_Count = 1
            then
               return "Select_Bounded_By_"
                 & Kit.Names.Ada_Name (Key_Name);
            else
               return "Select_"
                 & Kit.Names.Ada_Name (Key_Name)
                 & "_Bounded_By_"
                 & Key.Field (Bounded_Index).Ada_Name;
            end if;
         else
            return (if Key_Value
                    then "Select_By_"
                    else "Scan_By_")
              & Kit.Names.Ada_Name (Key_Name);
         end if;
      end Function_Name;

      ----------------
      -- To_Storage --
      ----------------

      function To_Storage
        (First       : Boolean)
         return Expression'Class
      is
      begin
         if not Bounds then
            return Table.To_Storage
              (Table, Key_Table, "", Key,
               With_Index => False);
         elsif First then
            return Table.To_Storage
              (Table, Key_Table,
               "Start_", Key,
               With_Index => False,
               Last_Index => Bounded_Index,
               Fill_Low => True);
         else
            return Table.To_Storage
              (Table, Key_Table,
               "Finish_", Key,
               With_Index => False,
               Last_Index => Bounded_Index,
               Fill_Low   => False);
         end if;
      end To_Storage;

   begin

      Check_Deferred_Keys (Function_Body, Key_Table);

      declare
         use Syn.Declarations;
         Block        : Syn.Blocks.Block_Type;
         Return_Type  : constant String := "Selection";
         Marlowe_Key  : constant String :=
           "Marlowe_Keys."
           & Table.Key_Reference_Name (Key_Name);
         Start_Storage    : constant Expression'Class :=
           New_Function_Call_Expression
             ("Marlowe.Key_Storage.To_Storage_Array",
              "Marlowe.Database_Index'First");
         Last_Storage     : constant Expression'Class :=
           New_Function_Call_Expression
             ("Marlowe.Key_Storage.To_Storage_Array",
              "Marlowe.Database_Index'Last");
         First_Key        : constant String := "others => 0";
         Last_Key         : constant String :=
                              "others => "
                              & "System.Storage_Elements.Storage_Element'Last";
         Aggregate_Start  : constant Character :=
                              (if Option_Ada_2022
                               then '['
                               else '(');
         Aggregate_End   : constant Character :=
                              (if Option_Ada_2022
                               then ']'
                               else ')');
         First_Key_Init   : constant String :=
                              Aggregate_Start
                              & First_Key
                              & Aggregate_End;
         Last_Key_Init    : constant String :=
                              Aggregate_Start
                              & Last_Key
                              & Aggregate_End;
      begin
         Block.Add_Declaration
           (Use_Package ("System.Storage_Elements"));

         if Key_Value then
            Block.Add_Declaration
              (New_Constant_Declaration
                 (Name        => "First_Key",
                  Object_Type => "Storage_Array",
                  Value       =>
                    Operator
                      (Name  => "&",
                       Left  => To_Storage (True),
                       Right => Start_Storage)));
            Block.Add_Declaration
              (New_Constant_Declaration
                 (Name        => "Last_Key",
                  Object_Type => "Storage_Array",
                  Value       =>
                    Operator
                      (Name  => "&",
                       Left  => To_Storage (False),
                       Right => Last_Storage)));
         else
            Block.Add_Declaration
              (New_Constant_Declaration
                 (Name        => "First_Key",
                  Object_Type => "Storage_Array (1 .." & Key.Size'Image & ")",
                  Value       => Object (First_Key_Init)));
            Block.Add_Declaration
              (New_Constant_Declaration
                 (Name        => "Last_Key",
                  Object_Type => "Storage_Array (1 .." & Key.Size'Image & ")",
                  Value       => Object (Last_Key_Init)));
         end if;

         declare
            Initialiser      : Function_Call_Expression :=
              New_Function_Call_Expression
                ("Marlowe_Keys.Handle.Search");
         begin
            Initialiser.Add_Actual_Argument
              (Object (Marlowe_Key));
            Initialiser.Add_Actual_Argument
              (Object ("First_Key"));
            Initialiser.Add_Actual_Argument
              (Object ("Last_Key"));

            Initialiser.Add_Actual_Argument
              (Object ("Marlowe.Closed"));
            Initialiser.Add_Actual_Argument
              (Object ("Marlowe.Closed"));

            Initialiser.Add_Actual_Argument
              (Object ("Marlowe.Forward"));

            Block.Add_Declaration
              (New_Object_Declaration
                 ("Mark", Data_Store_Cursor_Name,
                  Initialiser));
         end;

         declare
            While_Body : Sequence_Of_Statements;
         begin
            While_Body.Append
              (New_Procedure_Call_Statement
                 ("Result.Elements.Append",
                  New_Function_Call_Expression
                    (Table.Reference_Type_Name,
                     New_Function_Call_Expression
                       ("Marlowe.Key_Storage.To_Database_Index",
                        Object ("Mark.Get_Key")))));
            While_Body.Append
              (New_Procedure_Call_Statement
                 ("Mark.Next"));

            Return_Sequence.Append
              (While_Statement
                 (Object ("Mark.Valid"),
                  While_Body));
         end;

         Block.Append
           (Syn.Statements.New_Return_Statement
              ("Result", Return_Type, Return_Sequence));

         Function_Body.Append (Declare_Statement (Block));
      end;

      declare
         use Syn.Declarations;
         Fn : Subprogram_Declaration'Class :=
           New_Function
             (Function_Name, "Selection",
              Function_Body);
      begin
         if Key_Value then
            declare
               Key : constant Kit.Schema.Keys.Key_Type :=
                 Table.Key (Key_Name);
            begin
               if Bounds and then Bounded_Index = 0 then
                  for Is_Finish in Boolean loop
                     for I in 1 .. Field_Count loop
                        declare
                           Tag        : constant String :=
                             (if Is_Finish
                              then "Finish_"
                              else "Start_");
                           Field      : Kit.Schema.Fields.Field_Type
                           renames Key.Field (I);
                           Field_Type : Kit.Schema.Types.Kit_Type
                           renames Field.Get_Field_Type;
                        begin
                           if I = 1
                             or else Field_Type.Is_Table_Reference
                             or else Field_Type.Is_External_Type
                           then
                              Fn.Add_Formal_Argument
                                (New_Formal_Argument
                                   (Tag & Field.Ada_Name,
                                    Named_Subtype
                                      (Field_Type.Argument_Subtype)));
                           else
                              Fn.Add_Formal_Argument
                                (New_Formal_Argument
                                   (Tag & Field.Ada_Name,
                                    Named_Subtype
                                      (Field_Type.Argument_Subtype),
                                    (if Is_Finish
                                     then Field_Type.Last_Value
                                     else Field_Type.First_Value)));
                           end if;
                        end;
                     end loop;
                  end loop;
               else
                  for I in 1 .. Field_Count loop
                     declare
                        Field : Kit.Schema.Fields.Field_Type
                        renames Key.Field (I);
                     begin
                        if Bounds and then I = Bounded_Index then
                           Fn.Add_Formal_Argument
                             (New_Formal_Argument
                                ("Start_" & Field.Ada_Name,
                                 Named_Subtype
                                   (Field.Get_Field_Type
                                    .Argument_Subtype)));
                           Fn.Add_Formal_Argument
                             (New_Formal_Argument
                                ("Finish_" & Field.Ada_Name,
                                 Named_Subtype
                                   (Field.Get_Field_Type
                                    .Argument_Subtype)));
                        else
                           Fn.Add_Formal_Argument
                             (New_Formal_Argument
                                (Field.Ada_Name,
                                 Named_Subtype
                                   (Field.Get_Field_Type
                                    .Argument_Subtype)));
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end if;
         Table_Package.Append (Fn);
      end;

      Table_Package.Append (Syn.Declarations.New_Separator);
   end Create_Selection_Function;

   --------------------------------
   -- Create_Unique_Get_Function --
   --------------------------------

   procedure Create_Unique_Get_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String)
   is
   begin
      Create_Non_Iterator_Fetch
        (Table, Key_Table, Table_Package, Key_Name,
         Fetch_Type => Unique_Get);
   end Create_Unique_Get_Function;

end Kit.Generate.Public_Get;
