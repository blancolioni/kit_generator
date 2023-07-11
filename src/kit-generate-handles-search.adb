with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;

with Kit.Names;

with Kit.Schema.Fields;
with Kit.Schema.Types;

package body Kit.Generate.Handles.Search is

   type Non_Iterator_Fetch_Type is (Unique_Get, First, Last);

   procedure Create_Iterator
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Non_Iterator_Fetch
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String;
      Fetch_Type    : Non_Iterator_Fetch_Type);

   --  procedure Create_Iterator_Start_Function
   --    (Table         : in     Kit.Schema.Tables.Table_Type;
   --     Table_Package : in out Syn.Declarations.Package_Type'Class;
   --     Container     : in     Boolean;
   --     First         : in     Boolean)
   --  is null;
   --
   --  procedure Create_Iterator_Next_Function
   --    (Table         : in     Kit.Schema.Tables.Table_Type;
   --     Table_Package : in out Syn.Declarations.Package_Type'Class;
   --     With_Iterator : in     Boolean;
   --     Inline        : in     Boolean;
   --     Next          : in     Boolean)
   --    is null;

   function Local_Argument_Subtype
     (Db         : Kit.Schema.Databases.Database_Type;
      Table      : Kit.Schema.Tables.Table_Type;
      Field_Type : Kit.Schema.Types.Kit_Type)
      return Syn.Subtype_Indication'Class;

   ----------------------------------
   -- Create_Default_Key_Functions --
   ----------------------------------

   procedure Create_Default_Key_Functions
     (Db         : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
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

      Block.Add_Declaration
        (Syn.Declarations.New_Constant_Declaration
           (Name        => "Handle",
            Object_Type => Table.Ada_Name & "_Handle",
            Value       => Ask));
      Block.Add_Statement
        (Syn.Statements.New_Return_Statement
           (Syn.Object ("Handle.Has_Element")));

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
                     Local_Argument_Subtype
                       (Db, Table, Field.Get_Field_Type)));
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
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String)
   is
   begin
      Create_Non_Iterator_Fetch
        (Db, Table, Key_Table, Table_Package, Key_Name,
         Fetch_Type => First);
      Create_Non_Iterator_Fetch
        (Db, Table, Key_Table, Table_Package, Key_Name,
         Fetch_Type => Last);
   end Create_First_Last_Functions;

   ---------------------
   -- Create_Iterator --
   ---------------------

   procedure Create_Iterator
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is

      use Syn.Declarations, Syn.Types;

      Db_Table_Package : constant String :=
                     Db.Database_Package_Name
                           & "." & Table.Ada_Name;

      procedure Create_Iterator_Start_Function
        (Table_Package : in out Syn.Declarations.Package_Type'Class;
         First         : Boolean);

      procedure Create_Iterator_Next_Function
        (Table_Package : in out Syn.Declarations.Package_Type'Class;
         Next          : Boolean);

      -----------------------------------
      -- Create_Iterator_Next_Function --
      -----------------------------------

      procedure Create_Iterator_Next_Function
        (Table_Package : in out Syn.Declarations.Package_Type'Class;
         Next          : Boolean)
      is
         Name  : constant String := (if Next then "Next" else "Previous");
         Seq   : Syn.Statements.Sequence_Of_Statements;
         Block : Syn.Blocks.Block_Type;
      begin

         Seq.Append
           (Syn.Statements.New_Assignment_Statement
              ("Result.Db",
               Syn.Expressions.New_Function_Call_Expression
                 (Db_Table_Package & "." & Name,
                  "Position.Db")));

         Block.Add_Declaration
           (Syn.Declarations.New_Pragma
              ("Unreferenced", "It"));

         Block.Append
           (Syn.Statements.New_Return_Statement
              (Return_Variable   => "Result",
               Variable_Type     => "Cursor",
               Return_Statements => Seq));

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => Name,
                 Result_Type => "Cursor",
                 Argument_1  =>
                   Syn.Declarations.New_Formal_Argument
                     ("It", Syn.Named_Subtype ("Iterator")),
                 Argument_2  =>
                   Syn.Declarations.New_Formal_Argument
                     ("Position", Syn.Named_Subtype ("Cursor")),
                 Block       => Block);
         begin
            Fn.Set_Overriding;
            Table_Package.Append_To_Body (Fn);
         end;
      end Create_Iterator_Next_Function;

      ------------------------------------
      -- Create_Iterator_Start_Function --
      ------------------------------------

      procedure Create_Iterator_Start_Function
        (Table_Package : in out Syn.Declarations.Package_Type'Class;
         First         : Boolean)
      is
         Name  : constant String := (if First then "First" else "Last");
         Seq   : Syn.Statements.Sequence_Of_Statements;
         Block : Syn.Blocks.Block_Type;
      begin

         Seq.Append
           (Syn.Statements.New_Assignment_Statement
              ("Position.Db",
               Syn.Expressions.New_Function_Call_Expression
                 (Db_Table_Package & "." & Name,
                  Syn.Object ("It.Container.Db"))));

         Block.Append
           (Syn.Statements.New_Return_Statement
              (Return_Variable   => "Position",
               Variable_Type     => "Cursor",
               Return_Statements => Seq));

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => Name,
                 Result_Type => "Cursor",
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("It", Syn.Named_Subtype ("Iterator")),
                 Block       => Block);
         begin
            Fn.Set_Overriding;
            Table_Package.Append_To_Body (Fn);
         end;
      end Create_Iterator_Start_Function;

      Iterator_Definition : Record_Type_Definition;

   begin

      Iterator_Definition.Add_Parent
        ("Selection_Iterator_Interfaces.Reversible_Iterator");

      Iterator_Definition.Add_Component
        (Component_Name => "Container",
         Component_Type => Table.Ada_Name & "_Selection",
         Is_Access      => True,
         Is_Constant    => True);

      Table_Package.Append_To_Body
        (New_Full_Type_Declaration
           ("Iterator", Iterator_Definition));

      for First in reverse Boolean loop
         Create_Iterator_Start_Function
           (Table_Package, First);
      end loop;

      for Next in reverse Boolean loop
         Create_Iterator_Next_Function
           (Table_Package, Next);
      end loop;

   end Create_Iterator;

   -------------------------------
   -- Create_Non_Iterator_Fetch --
   -------------------------------

   procedure Create_Non_Iterator_Fetch
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String;
      Fetch_Type    : Non_Iterator_Fetch_Type)
   is
      pragma Unreferenced (Key_Table);
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions, Syn.Statements;

      Key              : constant Kit.Schema.Keys.Key_Type :=
                           Table.Key (Key_Name);

      Block            : Syn.Blocks.Block_Type;

      function Function_Name return String;
      function Call_Db_Function return Function_Call_Expression'Class;

      ----------------------
      -- Call_Db_Function --
      ----------------------

      function Call_Db_Function return Function_Call_Expression'Class is
         Call : Function_Call_Expression'Class :=
                  New_Function_Call_Expression
                    (Db.Database_Package_Name
                     & "."
                     & Table.Ada_Name
                     & "."
                     & Function_Name);
      begin
         for I in 1 .. Key.Field_Count loop
            declare
               Field : Kit.Schema.Fields.Field_Type
               renames Key.Field (I);
            begin
               if Field.Get_Field_Type.Is_Table_Reference then
                  Call.Add_Actual_Argument
                    (Object
                       (Field.Ada_Name & ".Reference_"
                        & Field.Get_Field_Type.Ada_Name));
               else
                  Call.Add_Actual_Argument
                    (Object (Field.Ada_Name));
               end if;
            end;
         end loop;
         return Call;
      end Call_Db_Function;

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

   begin
      Block.Add_Declaration
        (Use_Package
           (Db.Database_Package_Name));

      Block.Add_Declaration
        (New_Constant_Declaration
           (Name        => "Ref",
            Object_Type =>
              Table.Ada_Name & "_Reference",
            Value       => Call_Db_Function));
      Block.Add_Declaration
        (New_Object_Declaration
           (Name        => "Result",
            Object_Type => Table.Ada_Name & "_Handle"));
      Block.Append
        (Syn.Statements.If_Statement
           (Condition =>
                Operator
              ("/=", Object ("Ref"),
               Object ("Null_" & Table.Ada_Name & "_Reference")),
            True_Part =>
              New_Assignment_Statement
                (Target    => "Result",
                 Value     =>
                   New_Function_Call_Expression
                     ("Get", Object ("Ref")))));
      Block.Append
        (New_Return_Statement
           (Object ("Result")));

      declare
         Fn          : Subprogram_Declaration'Class :=
                         New_Function
                           (Function_Name,
                            Table.Ada_Name & "_Handle",
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
                     Local_Argument_Subtype
                       (Db, Table, Field.Get_Field_Type)));
            end;
         end loop;
         Table_Package.Append (Fn);
      end;

      Table_Package.Append (Syn.Declarations.New_Separator);
   end Create_Non_Iterator_Fetch;

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
      Bounded_Index : Natural := 0)
   is
      pragma Unreferenced (Key_Table);
      use Syn;
      use Syn.Expressions, Syn.Statements;

      Scanning : constant Boolean :=
                   not Bounds and then not Key_Value
                       with Unreferenced;

      Key              : constant Kit.Schema.Keys.Key_Type :=
                           Table.Key (Key_Name);
      Field_Count      : constant Positive :=
                           (if not Bounds
                            or else Bounded_Index = 0
                            then Key.Field_Count
                            else Bounded_Index);

      function Function_Name return String;
      function Call_Db_Function return Function_Call_Expression'Class;

      ----------------------
      -- Call_Db_Function --
      ----------------------

      function Call_Db_Function return Function_Call_Expression'Class is
         Call : Function_Call_Expression'Class :=
                  New_Function_Call_Expression
                    (Db.Database_Package_Name
                     & "."
                     & Table.Ada_Name
                     & "."
                     & Function_Name);

         function Pass_Field
           (Field : Kit.Schema.Fields.Field_Type;
            Tag   : String := "")
            return Expression'Class
         is (Object
             (if Field.Get_Field_Type.Is_Table_Reference
              then Tag & Field.Ada_Name & ".Reference_"
              & Field.Get_Field_Type.Ada_Name
              else Tag & Field.Ada_Name));

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
                        begin
                           Call.Add_Actual_Argument
                             (Pass_Field (Field, Tag));
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
                           Call.Add_Actual_Argument
                             (Pass_Field (Field, "Start_"));
                           Call.Add_Actual_Argument
                             (Pass_Field (Field, "Finish_"));
                        else
                           Call.Add_Actual_Argument
                             (Pass_Field (Field));
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end if;

         return Call;

      end Call_Db_Function;

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

      Block : Syn.Blocks.Block_Type;

   begin

      Block.Add_Declaration
        (Syn.Declarations.New_Constant_Declaration
           (Name        => "Db_Selection",
            Object_Type =>
              Db.Database_Package_Name
            & "."
            & Table.Ada_Name
            & "."
            & "Selection",
            Value       => Call_Db_Function));

      Block.Append
        (New_Return_Statement
           (Object ("(Db => Db_Selection)")));

      declare
         use Syn.Declarations;
         Fn : Subprogram_Declaration'Class :=
                New_Function
                  (Function_Name,
                   Table.Ada_Name & "_Selection",
                   Block);
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
                                    Local_Argument_Subtype
                                      (Db, Table, Field_Type)));
                           else
                              Fn.Add_Formal_Argument
                                (New_Formal_Argument
                                   (Tag & Field.Ada_Name,
                                    Local_Argument_Subtype
                                      (Db, Table, Field_Type),
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
                                 Local_Argument_Subtype
                                   (Db, Table, Field.Get_Field_Type)));
                           Fn.Add_Formal_Argument
                             (New_Formal_Argument
                                ("Finish_" & Field.Ada_Name,
                                 Local_Argument_Subtype
                                   (Db, Table, Field.Get_Field_Type)));
                        else
                           Fn.Add_Formal_Argument
                             (New_Formal_Argument
                                (Field.Ada_Name,
                                 Local_Argument_Subtype
                                   (Db, Table, Field.Get_Field_Type)));
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

   ---------------------------
   -- Create_Selection_Type --
   ---------------------------

   procedure Create_Selection_Type
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)

   is

      use Syn;
      use Syn.Declarations;

      Db_Table_Ref : constant String :=
                       Db.Database_Package_Name
                       & "."
                       & Table.Ada_Name;
      Selection_Name : constant String :=
                         Table.Ada_Name & "_Selection";
      Selection          : Syn.Types.Record_Type_Definition;
      Cursor             : Syn.Types.Record_Type_Definition;
      Constant_Reference : Syn.Types.Record_Type_Definition;
      Variable_Reference : Syn.Types.Record_Type_Definition;
      Iterator_Package   : Syn.Declarations.Package_Type :=
                             Syn.Declarations.New_Package_Type
                               ("Selection_Iterator_Interfaces");

   begin

      Table_Package.With_Package ("Ada.Iterator_Interfaces");
      Table_Package.With_Package
        (Db.Database_Package_Name & "." & Table.Ada_Name,
         Private_With => True);

      Cursor.Add_Component
        ("Db",
         Db.Database_Package_Name & "." & Table.Ada_Name & ".Cursor");

      Table_Package.Append
        (New_Private_Type_Declaration
           ("Cursor", Cursor));

      declare
         use Syn.Expressions;
         use Syn.Statements;
         Has_Element_Block : Syn.Blocks.Block_Type;
      begin
         Has_Element_Block.Add_Statement
           (New_Return_Statement
              (New_Function_Call_Expression
                   (Db_Table_Ref & ".Has_Element",
                    Object ("Item.Db"))));

         Table_Package.Append
           (New_Function
              (Name        => "Has_Element",
               Argument    =>
                 New_Formal_Argument ("Item", Named_Subtype ("Cursor")),
               Result_Type => "Boolean",
               Block       => Has_Element_Block));
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
         --  Table_Package.Append (Ref_Type);
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
         --  Table_Package.Append (Ref_Type);
      end;

      Selection.Set_Tagged;
      Selection.Add_Component
        ("Db",
         Db_Table_Ref & ".Selection");
      Iterator_Package.Set_Generic_Instantiation
        ("Ada.Iterator_Interfaces");
      Iterator_Package.Add_Generic_Actual_Argument ("Cursor");
      Iterator_Package.Add_Generic_Actual_Argument ("Has_Element");
      Table_Package.Append (Iterator_Package);

      declare
         Selection_Type : Type_Declaration :=
                            New_Private_Type_Declaration
                              (Selection_Name, Selection);
      begin
         Selection_Type.Add_Aspect ("Constant_Indexing",
                                    "Constant_Reference");
         --  Selection_Type.Add_Aspect ("Variable_Indexing",
         --                             "Variable_Reference");
         Selection_Type.Add_Aspect ("Default_Iterator",
                                    "Iterate");
         Selection_Type.Add_Aspect ("Iterator_Element",
                                    Table.Ada_Name & "_Class");

         Table_Package.Append (Selection_Type);
      end;

      declare
         use Syn.Statements;
         Iterate_Block   : Syn.Blocks.Block_Type;
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
              (Name        => "Iterate",
               Argument    =>
                 New_In_Argument
                   ("Container",
                    Named_Subtype (Selection_Name)),
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
                   Syn.Expressions.New_Function_Call_Expression
                 ("Get",
                  Syn.Expressions.New_Function_Call_Expression
                    (Db.Database_Package_Name
                     & "." & Table.Ada_Name
                     & ".Element",
                     Object ("Position.Db")))));
         declare
            Fn_Name   : constant String :=
                          "Constant_Reference";
            Ref_Fn    : Subprogram_Declaration'Class :=
                          New_Function
                            (Fn_Name,
                             Named_Subtype
                               (Table.Ada_Name & "_Class"),
                             Ref_Block);
            Container : Formal_Argument'Class :=
                          New_Formal_Argument
                            ("Container",
                             In_Argument,
                             Named_Subtype
                               (Selection_Name));
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
                                Named_Subtype (Selection_Name)),
                         Result_Type => "Boolean",
                         Block       =>
                           Syn.Blocks.Create_Block
                             (New_Return_Statement
                                  (New_Function_Call_Expression
                                       (Db_Table_Ref & ".Is_Empty",
                                        Object ("Container.Db")))));
      begin
         Table_Package.Append (Is_Empty);
      end;

      declare
         use Syn.Expressions, Syn.Statements;
         Element : constant Subprogram_Declaration'Class :=
                     New_Function
                       (Name        => "Element",
                        Argument    =>
                          New_In_Argument
                            (Name          => "Item",
                             Argument_Type =>
                               Named_Subtype ("Cursor")),
                        Result_Type =>
                          Table.Ada_Name & "_Class",
                        Block       =>
                          Syn.Blocks.Create_Block
                            (New_Return_Statement
                                 (New_Function_Call_Expression
                                      ("Get",
                                       New_Function_Call_Expression
                                         (Db_Table_Ref & ".Element",
                                          "Item.Db")))));
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
                      Named_Subtype (Selection_Name)),
               Result_Type => "Natural",
               Block       =>
                 Syn.Blocks.Create_Block
                   (New_Return_Statement
                        (New_Function_Call_Expression
                           (Db_Table_Ref & ".Length",
                            Object ("Container.Db"))))));
      end;

      Create_Iterator (Db, Table, Table_Package);

   end Create_Selection_Type;

   --------------------------------
   -- Create_Unique_Get_Function --
   --------------------------------

   procedure Create_Unique_Get_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String)
   is
   begin
      Create_Non_Iterator_Fetch
        (Db, Table, Key_Table, Table_Package, Key_Name,
         Fetch_Type => Unique_Get);
   end Create_Unique_Get_Function;

   ----------------------------
   -- Local_Argument_Subtype --
   ----------------------------

   function Local_Argument_Subtype
     (Db         : Kit.Schema.Databases.Database_Type;
      Table      : Kit.Schema.Tables.Table_Type;
      Field_Type : Kit.Schema.Types.Kit_Type)
      return Syn.Subtype_Indication'Class
   is
   begin
      if Field_Type.Is_Table_Reference
        and then Field_Type.Ada_Name /= Table.Ada_Name
      then
         return Syn.Named_Subtype
           (Db.Handle_Package_Name
            & "." & Field_Type.Ada_Name
            & "." & Field_Type.Argument_Handle_Subtype);
      elsif Field_Type.Ada_Name = "Record_Type" then
         return Syn.Named_Subtype
           (Db.Database_Package_Name
            & "." & Field_Type.Ada_Name);
      else
         return Syn.Named_Subtype (Field_Type.Argument_Handle_Subtype);
      end if;
   end Local_Argument_Subtype;

end Kit.Generate.Handles.Search;
