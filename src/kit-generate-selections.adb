with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;

with Kit.Schema.Fields;
with Kit.Schema.Keys;
with Kit.Schema.Types;

package body Kit.Generate.Selections is

   function Condition_Operator_Function
     (Operator : Kit.Schema.Types.Kit_Operator;
      Field    : Kit.Schema.Fields.Field_Type)
      return Syn.Declaration'Class;

   function Condition_Function
     (Field    : Kit.Schema.Fields.Field_Type)
      return Syn.Declaration'Class;

   procedure Scan_Key_Fields
     (Key     : Kit.Schema.Keys.Key_Type;
      Process : not null access
        procedure (Field : Kit.Schema.Fields.Field_Type;
                   Value_Name : String));

   function Constraint_Check
     (Operator         : Kit.Schema.Types.Kit_Operator;
      Field_Value      : String;
      Constraint_Value : String)
      return Syn.Expression'Class;

   function Operator_Function
     (Operator : Kit.Schema.Types.Kit_Operator)
      return String;

   function Constraint_Operator
     (Operator : Kit.Schema.Types.Kit_Operator)
      return String;

   procedure Create_Iterator
     (Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Selection_Type
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Selection_Key_Enumeration
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Selection_Key_Type
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Selection_Key_Holder
     (Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Constraint_Field_Enumeration
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Constraint_Field_Type
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Constraint_Field_List
     (Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Constraint_Check
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Field_Singleton_Types
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Key_Condition_Functions
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Condition_Merge_Functions
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Select_All_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Select_Where_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_First_Where_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Selection_Condition_Type
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Selection_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   function Ignore_Key
     (Key : Kit.Schema.Keys.Key_Type)
      return Boolean
   is (Key.Base_Reference or else Key.Ada_Name = "Top_Record");

   ------------------------
   -- Condition_Function --
   ------------------------

   function Condition_Function
     (Field    : Kit.Schema.Fields.Field_Type)
      return Syn.Declaration'Class
   is
      Block : Syn.Blocks.Block_Type;
      Ret_Seq : Syn.Statements.Sequence_Of_Statements;
   begin

      Block.Add_Declaration
        (Syn.Declarations.New_Object_Declaration
           (Name        => "Constraint",
            Object_Type =>
              "Selection_Constraint (C_" & Field.Ada_Name & ")"));
      Block.Append
        (Syn.Statements.New_Assignment_Statement
           ("Constraint.Operator", Syn.Object ("Operation")));

      Field.Get_Field_Type.Set_Value
        (Target_Name => "Constraint." & Field.Ada_Name & "_Value",
         Value_Name  => "Value",
         Sequence    => Block);

      Ret_Seq.Append
        (Syn.Statements.New_Assignment_Statement
           ("Result.Main_Key",
            Syn.Expressions.New_Function_Call_Expression
              ("Selection_Key_Holders.To_Holder",
               Syn.Object ("(K_No_Selection_Key, False)"))));

      Ret_Seq.Append
        (Syn.Statements.New_Procedure_Call_Statement
           ("Result.Constraints.Append",
            Syn.Object ("Constraint")));

      Block.Append
        (Syn.Statements.New_Return_Statement
           (Return_Variable   => "Result",
            Variable_Type     => "Selection_Condition",
            Return_Statements => Ret_Seq));

      declare
         Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
           Syn.Declarations.New_Function
             (Name        => Field.Ada_Name & "_Condition",
              Argument_1  =>
                Syn.Declarations.New_Formal_Argument
                  ("Operation", Syn.Named_Subtype ("Constraint_Operator")),
              Argument_2  =>
                Syn.Declarations.New_Formal_Argument
                  ("Value",
                   Syn.Named_Subtype
                     (Field.Get_Field_Type.Argument_Subtype)),
              Result_Type => "Selection_Condition",
              Block       => Block);
      begin
         return Fn;
      end;

   end Condition_Function;

   ---------------------------------
   -- Condition_Operator_Function --
   ---------------------------------

   function Condition_Operator_Function
     (Operator : Kit.Schema.Types.Kit_Operator;
      Field    : Kit.Schema.Fields.Field_Type)
      return Syn.Declaration'Class
   is
      Block : Syn.Blocks.Block_Type;
   begin
      Block.Add_Declaration
        (Syn.Declarations.New_Pragma ("Unreferenced", "Left"));
      Block.Append
        (Syn.Statements.New_Return_Statement
           (Syn.Expressions.New_Function_Call_Expression
                (Field.Ada_Name & "_Condition",
                 Constraint_Operator (Operator),
                 (if Field.Get_Field_Type.Is_Table_Reference
                  then "Right.Reference_" & Field.Get_Field_Type.Ada_Name
                  else "Right"))));

      declare
         use all type Kit.Schema.Types.Kit_Operator;
         Fn : Syn.Declarations.Subprogram_Declaration'Class :=
           Syn.Declarations.New_Function
             (Name        => Operator_Function (Operator),
              Argument  =>
                Syn.Declarations.New_Formal_Argument
                  ("Left", Syn.Named_Subtype (Field.Ada_Name & "_Field")),
              Result_Type => "Selection_Condition",
              Block       => Block);
      begin
         if Operator not in Is_False | Is_True then
            Fn.Add_Formal_Argument
              (Syn.Declarations.New_Formal_Argument
                 ("Right",
                  Syn.Named_Subtype
                    (Field.Get_Field_Type.Argument_Handle_Subtype)));
         end if;
         return Fn;
      end;

   end Condition_Operator_Function;

   ----------------------
   -- Constraint_Check --
   ----------------------

   function Constraint_Check
     (Operator         : Kit.Schema.Types.Kit_Operator;
      Field_Value      : String;
      Constraint_Value : String)
      return Syn.Expression'Class
   is
      use all type Kit.Schema.Types.Kit_Operator;

      function Op (Op_Name : String) return Syn.Expression'Class
      is (Syn.Expressions.Operator
          (Op_Name,
           Syn.Object (Field_Value), Syn.Object (Constraint_Value)));

   begin
      case Operator is
         when Is_False =>
            return Syn.Object (Field_Value);
         when Is_True =>
            return Syn.Expressions.Operator ("not", Syn.Object (Field_Value));
         when EQ =>
            return Op ("/=");
         when NE =>
            return Op ("=");
         when LE =>
            return Op (">");
         when GE =>
            return Op ("<");
         when LT =>
            return Op (">=");
         when GT =>
            return Op ("<=");
      end case;
   end Constraint_Check;

   -------------------------
   -- Constraint_Operator --
   -------------------------

   function Constraint_Operator
     (Operator : Kit.Schema.Types.Kit_Operator)
      return String
   is
      use all type Kit.Schema.Types.Kit_Operator;
   begin
      case Operator is
         when Is_False =>
            return "Op_Not";
         when Is_True =>
            return "Op_None";
         when EQ =>
            return "Op_EQ";
         when NE =>
            return "Op_NE";
         when LE =>
            return "Op_LE";
         when GE =>
            return "Op_GE";
         when LT =>
            return "Op_LT";
         when GT =>
            return "Op_GT";
      end case;
   end Constraint_Operator;

   --------------------------------------
   -- Create_Condition_Merge_Functions --
   --------------------------------------

   procedure Create_Condition_Merge_Functions
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      Block     : Syn.Blocks.Block_Type;
      Ret_Seq   : Syn.Statements.Sequence_Of_Statements;

      function Better_Key_Function return Syn.Declaration'Class;
      function To_Constraint_Function return Syn.Declaration'Class;

      function Copy_Constraints
        (From : String)
         return Syn.Statement'Class;

      function Copy_Main
        (Choice, Other : String)
         return Syn.Statements.Sequence_Of_Statements;

      -------------------------
      -- Better_Key_Function --
      -------------------------

      function Better_Key_Function return Syn.Declaration'Class is
         Block : Syn.Blocks.Block_Type;
      begin

         Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("Left_Key", "Selection_Key",
               Syn.Object ("Left.Main_Key.Element")));

         Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("Right_Key", "Selection_Key",
               Syn.Object ("Right.Main_Key.Element")));

         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.Operator
                   ("or else",
                    Syn.Expressions.Operator
                      ("or else",
                       Syn.Expressions.Operator
                         ("=",
                          Syn.Object ("Right_Key.Key_Type"),
                          Syn.Object ("K_No_Selection_Key")),
                       Syn.Expressions.Operator
                         ("not",
                          Syn.Object ("Right_Key.Has_Value"))),
                    Syn.Expressions.Operator
                      ("and then",
                       Syn.Object ("Left_Key.Has_Value"),
                       Syn.Expressions.Operator
                         (">",
                          Syn.Object ("Left_Key.Key_Type"),
                          Syn.Object ("Right_Key.Key_Type"))))));

         return Syn.Declarations.New_Function
           ("Better_Key",
            Argument_1  =>
              Syn.Declarations.New_Formal_Argument
                ("Left", Syn.Named_Subtype ("Selection_Condition")),
            Argument_2  =>
              Syn.Declarations.New_Formal_Argument
                ("Right", Syn.Named_Subtype ("Selection_Condition")),
            Result_Type => "Boolean",
            Block       => Block);
      end Better_Key_Function;

      ----------------------
      -- Copy_Constraints --
      ----------------------

      function Copy_Constraints
        (From : String)
         return Syn.Statement'Class
      is
      begin
         return Syn.Statements.Iterate
           ("Constraint", From & ".Constraints",
            Syn.Statements.New_Procedure_Call_Statement
              ("Result.Constraints.Append",
               Syn.Object ("Constraint")));
      end Copy_Constraints;

      ---------------
      -- Copy_Main --
      ---------------

      function Copy_Main
        (Choice, Other : String)
         return Syn.Statements.Sequence_Of_Statements
      is
      begin
         return Seq : Syn.Statements.Sequence_Of_Statements do
            Seq.Append
              (Syn.Statements.New_Assignment_Statement
                 ("Result.Main_Key", Syn.Object (Choice & ".Main_Key")));
            Seq.Append
              (Syn.Statements.New_Assignment_Statement
                 ("Result.Constraints",
                  Syn.Expressions.New_Function_Call_Expression
                    ("To_Constraints",
                     Syn.Object (Other & ".Main_Key.Element"))));
         end return;
      end Copy_Main;

      ----------------------------
      -- To_Constraint_Function --
      ----------------------------

      function To_Constraint_Function return Syn.Declaration'Class is

         Case_Statement : Syn.Statements.Case_Statement_Record'Class :=
           Syn.Statements.Case_Statement ("Key.Key_Type");

         procedure Add_Case
           (Base : Kit.Schema.Tables.Table_Type;
            Key  : Kit.Schema.Keys.Key_Type);

         --------------
         -- Add_Case --
         --------------

         procedure Add_Case
           (Base : Kit.Schema.Tables.Table_Type;
            Key  : Kit.Schema.Keys.Key_Type)
         is
            pragma Unreferenced (Base);
            Block : Syn.Blocks.Block_Type;

            procedure Add_Field_Constraint
              (Field     : Kit.Schema.Fields.Field_Type;
               Key_Field : String);

            --------------------------
            -- Add_Field_Constraint --
            --------------------------

            procedure Add_Field_Constraint
              (Field     : Kit.Schema.Fields.Field_Type;
               Key_Field : String)
            is
               Constraint : constant String :=
                 Field.Ada_Name & "_Constraint";
            begin
               Block.Add_Declaration
                 (Syn.Declarations.New_Object_Declaration
                    (Constraint,
                     Syn.Named_Subtype
                       ("Selection_Constraint ("
                        & "C_" & Field.Ada_Name & ")")));
               Block.Add_Statement
                 (Syn.Statements.New_Assignment_Statement
                    (Constraint & ".Operator",
                     Syn.Object ("Op_EQ")));
               Block.Add_Statement
                 (Syn.Statements.New_Assignment_Statement
                    (Constraint & "." & Field.Ada_Name & "_Value",
                     Syn.Object ("Key." & Key_Field)));
               Block.Add_Statement
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("List.Append",
                     Syn.Object (Constraint)));
            end Add_Field_Constraint;

         begin
            if Ignore_Key (Key) then
               return;
            end if;

            Scan_Key_Fields (Key, Add_Field_Constraint'Access);

            Case_Statement.Add_Case_Option
              ("K_" & Key.Ada_Name,
               Syn.Statements.Declare_Statement (Block));

         end Add_Case;

         Ret_Seq : Syn.Statements.Sequence_Of_Statements;
         Block   : Syn.Blocks.Block_Type;
      begin

         Case_Statement.Add_Case_Option
           ("K_No_Selection_Key",
            Syn.Statements.New_Null_Statement);

         Table.Scan_Keys (Add_Case'Access);

         Ret_Seq.Append
           (Syn.Statements.If_Statement
              (Syn.Object ("Key.Has_Value"),
               Case_Statement));

         Block.Append
           (Syn.Statements.New_Return_Statement
              ("List",
               "Selection_Constraint_Lists.List",
               Ret_Seq));

         return Syn.Declarations.New_Function
           ("To_Constraints",
            Argument  =>
              Syn.Declarations.New_Formal_Argument
                ("Key", Syn.Named_Subtype ("Selection_Key")),
            Result_Type => "Selection_Constraint_Lists.List",
            Block       => Block);
      end To_Constraint_Function;

   begin
      Block.Add_Declaration
        (Syn.Declarations.New_Constant_Declaration
           ("Choose_Left_Key", "Boolean",
            Syn.Expressions.New_Function_Call_Expression
              ("Better_Key", Syn.Object ("Left"), Syn.Object ("Right"))));

      Ret_Seq.Append
        (Syn.Statements.If_Statement
           (Condition  => Syn.Object ("Choose_Left_Key"),
            True_Part  => Copy_Main ("Left", "Right"),
            False_Part => Copy_Main ("Right", "Left")));

      Ret_Seq.Append (Copy_Constraints ("Left"));
      Ret_Seq.Append (Copy_Constraints ("Right"));

      Block.Append
        (Syn.Statements.New_Return_Statement
           ("Result", "Selection_Condition", Ret_Seq));

      Table_Package.Append_To_Body (Better_Key_Function);
      Table_Package.Append_To_Body (To_Constraint_Function);

      Table_Package.Append
        (Syn.Declarations.New_Function
           (Name        => """and""",
            Argument_1  =>
              Syn.Declarations.New_Formal_Argument
                ("Left", Syn.Named_Subtype ("Selection_Condition")),
            Argument_2  =>
              Syn.Declarations.New_Formal_Argument
                ("Right", Syn.Named_Subtype ("Selection_Condition")),
            Result_Type => "Selection_Condition",
            Block       => Block));
      Table_Package.Add_Separator;
   end Create_Condition_Merge_Functions;

   -----------------------------
   -- Create_Constraint_Check --
   -----------------------------

   procedure Create_Constraint_Check
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is

      function Field_Case return Syn.Statement'Class;

      function Field_Case return Syn.Statement'Class is

         Result : Syn.Statements.Case_Statement_Record'Class :=
           Syn.Statements.Case_Statement ("Constraint.Field");

         procedure Add_Case
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type);

         --------------
         -- Add_Case --
         --------------

         procedure Add_Case
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type)
         is
            pragma Unreferenced (Base);
            Block : Syn.Blocks.Block_Type;
         begin
            if Field.Get_Field_Type.Is_Table_Reference
              or else Field.Get_Field_Type.Has_Custom_Type
            then
               Block.Add_Declaration
                 (Syn.Declarations.Use_Package (Db.Database_Package_Name));
            elsif Field.Get_Field_Type.Is_External_Type then
               Block.Add_Declaration
                 (Syn.Declarations.Use_Package
                    (Field.Get_Field_Type.External_Type_Package_Name));
            end if;

            Block.Add_Declaration
              (Syn.Declarations.New_Constant_Declaration
                 ("Value", Field.Get_Field_Type.Argument_Subtype,
                  Syn.Object
                    ("Item."
                     & (if Field.Base_Reference
                       then "Get_" & Field.Ada_Name & "_Reference"
                       else Field.Ada_Name))));
            Block.Add_Declaration
              (Syn.Declarations.New_Constant_Declaration
                 ("Compare", Field.Get_Field_Type.Argument_Subtype,
                  Field.Get_Field_Type.Return_Value
                    ("Constraint." & Field.Ada_Name & "_Value")));

            declare
               Operator_Case : Syn.Statements.Case_Statement_Record'Class :=
                 Syn.Statements.Case_Statement ("Constraint.Operator");

               procedure Add_Operator_Case
                 (Op : Kit.Schema.Types.Kit_Operator);

               procedure Add_Operator_Case
                 (Op : Kit.Schema.Types.Kit_Operator)
               is
               begin
                  Operator_Case.Add_Case_Option
                    (Constraint_Operator (Op),
                     Syn.Statements.If_Statement
                       (Constraint_Check (Op, "Value", "Compare"),
                        Syn.Statements.New_Return_Statement
                          (Syn.Object ("False"))));
               end Add_Operator_Case;

            begin
               for Op in Kit.Schema.Types.Kit_Operator loop
                  if not Field.Get_Field_Type.Has_Operator (Op) then
                     Operator_Case.Add_Case_Option
                       (Constraint_Operator (Op),
                        Syn.Statements.Raise_Statement
                          ("Program_Error",
                           "bad operator: "
                           & Constraint_Operator (Op)
                           & "/"
                           & Field.Ada_Name));
                  else
                     Add_Operator_Case (Op);
                  end if;
               end loop;
               Block.Append (Operator_Case);
            end;

            Result.Add_Case_Option
              ("C_" & Field.Ada_Name,
               Syn.Statements.Declare_Statement (Block));
         end Add_Case;

      begin
         Table.Iterate_All (Add_Case'Access);
         return Result;
      end Field_Case;

      Block : Syn.Blocks.Block_Type;
   begin
      Block.Append
        (Syn.Statements.Iterate
           ("Constraint", "Constraints", Field_Case));

      Block.Append
        (Syn.Statements.New_Return_Statement (Syn.Object ("True")));

      Table_Package.Append_To_Body
        (Syn.Declarations.New_Function
           (Name        => "Check_Constraints",
            Argument_1  =>
              Syn.Declarations.New_Formal_Argument
                ("Constraints",
                 Syn.Named_Subtype ("Selection_Constraint_Lists.List")),
            Argument_2  =>
              Syn.Declarations.New_Formal_Argument
                ("Item",
                 Syn.Named_Subtype
                   (Db.Database_Package_Name
                    & "." & Table.Ada_Name
                    & "." & Table.Ada_Name & "_Type")),
            Result_Type => "Boolean",
            Block       => Block));
   end Create_Constraint_Check;

   -----------------------------------------
   -- Create_Constraint_Field_Enumeration --
   -----------------------------------------

   procedure Create_Constraint_Field_Enumeration
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is

      Constraint_Enum : Syn.Enumeration_Type_Definition;

      procedure Add_Constraint_Literal
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      ----------------------------
      -- Add_Constraint_Literal --
      ----------------------------

      procedure Add_Constraint_Literal
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
      begin
         Constraint_Enum.New_Literal ("C_" & Field.Ada_Name);
      end Add_Constraint_Literal;

   begin
      Table.Iterate_All
        (Process     => Add_Constraint_Literal'Access);

      declare
         Enum : Syn.Declarations.Type_Declaration'Class :=
           Syn.Declarations.New_Full_Type_Declaration
             ("Constraint_Field", Constraint_Enum);
      begin
         Enum.Set_Private_Spec;
         Table_Package.Append (Enum);
      end;

   end Create_Constraint_Field_Enumeration;

   ----------------------------------
   -- Create_Constraint_Field_List --
   ----------------------------------

   procedure Create_Constraint_Field_List
     (Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      List_Package   : Syn.Declarations.Package_Type :=
        Syn.Declarations.New_Package_Type
          ("Selection_Constraint_Lists");
   begin
      List_Package.Set_Generic_Instantiation
        ("Ada.Containers.Indefinite_Doubly_Linked_Lists");
      List_Package.Add_Generic_Actual_Argument ("Selection_Constraint");
      List_Package.Set_Private_Spec;
      Table_Package.Append (List_Package);
   end Create_Constraint_Field_List;

   ----------------------------------
   -- Create_Constraint_Field_Type --
   ----------------------------------

   procedure Create_Constraint_Field_Type
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      function Constraint_Field_Record return Syn.Type_Definition'Class;

      -----------------------------
      -- Constraint_Field_Record --
      -----------------------------

      function Constraint_Field_Record return Syn.Type_Definition'Class is

         First     : Boolean := True;
         Field_Rec : Syn.Types.Record_Type_Definition;

         procedure Add_Field_Variant
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type);

         -----------------------
         -- Add_Field_Variant --
         -----------------------

         procedure Add_Field_Variant
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type)
         is
            pragma Unreferenced (Base);
            Literal_Name : constant String := "C_" & Field.Ada_Name;
         begin
            if First then
               Field_Rec.Start_Case (Literal_Name);
               First := False;
            else
               Field_Rec.Next_Case_Option (Literal_Name);
            end if;

            Field_Rec.Add_Component
              (Component_Name => Field.Ada_Name & "_Value",
               Component_Type => Field.Get_Field_Type.Record_Subtype);

         end Add_Field_Variant;

      begin
         Field_Rec.Add_Variant ("Field", "Constraint_Field");
         Field_Rec.Add_Component ("Operator", "Constraint_Operator");

         Table.Iterate_All
           (Process           => Add_Field_Variant'Access);

         return Field_Rec;
      end Constraint_Field_Record;

      Constraint_Type : Syn.Declarations.Type_Declaration'Class :=
        Syn.Declarations.New_Full_Type_Declaration
          ("Selection_Constraint", Constraint_Field_Record);
   begin
      Constraint_Type.Set_Private_Spec;
      Table_Package.Append (Constraint_Type);
   end Create_Constraint_Field_Type;

   ----------------------------------
   -- Create_Field_Singleton_Types --
   ----------------------------------

   procedure Create_Field_Singleton_Types
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is

      procedure Add_Singleton_Type
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      ------------------------
      -- Add_Singleton_Type --
      ------------------------

      procedure Add_Singleton_Type
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
         Singleton_Definition : Syn.Types.Record_Type_Definition;
      begin
         if Field.Ada_Name /= "Top_Record"
           and then Field.Ada_Name /= "Kit_Root_Record"
         then
            Table_Package.Append
              (Syn.Declarations.New_Private_Type_Declaration
                 (Field.Ada_Name & "_Field", Singleton_Definition));
            Table_Package.Add_Separator;

            declare
               Block : Syn.Blocks.Block_Type;
            begin
               Block.Append
                 (Syn.Statements.New_Return_Statement
                    (Syn.Object ("(null record)")));
               Table_Package.Append
                 (Syn.Declarations.New_Function
                    (Name        => Field.Ada_Name,
                     Result_Type => Field.Ada_Name & "_Field",
                     Block       => Block));
               Table_Package.Add_Separator;
            end;

            Table_Package.Append_To_Body
              (Condition_Function (Field));
            Table_Package.Append_To_Body
              (Syn.Declarations.New_Separator);

            for Operator in Kit.Schema.Types.Kit_Operator loop
               if Field.Get_Field_Type.Has_Operator (Operator) then
                  Table_Package.Append
                    (Condition_Operator_Function
                       (Operator, Field));
                  Table_Package.Add_Separator;
               end if;
            end loop;

         end if;
      end Add_Singleton_Type;

   begin
      Table.Iterate_All
        (Process     => Add_Singleton_Type'Access);
   end Create_Field_Singleton_Types;

   ---------------------------------
   -- Create_First_Where_Function --
   ---------------------------------

   procedure Create_First_Where_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      Seq_Loop  : Syn.Statements.Sequence_Of_Statements;
      Block     : Syn.Blocks.Block_Type;
   begin
      Seq_Loop.Append
        (Syn.Statements.If_Statement
           (Syn.Expressions.New_Function_Call_Expression
                ("Check_Constraints",
                 Syn.Object ("Condition.Constraints"),
                 Syn.Object ("Item")),
            Syn.Statements.New_Return_Statement
              (Syn.Expressions.New_Function_Call_Expression
                   (Procedure_Name => "Get",
                    Argument       =>
                      Syn.Object
                        ("Item.Get_" & Table.Ada_Name & "_Reference")))));

      Block.Append
        (Syn.Statements.Iterate
           (Loop_Variable  => "Item",
            Container_Name =>
              "Create_Selection ("
            & "Condition.Main_Key.Element"
            & ")",
            Iterate_Body   => Seq_Loop));

      Block.Append
        (Syn.Statements.New_Return_Statement
           (Syn.Object ("Empty_Handle")));

      Table_Package.Append
        (Syn.Declarations.New_Function
           (Name        => "First_Where",
            Argument    =>
              Syn.Declarations.New_Formal_Argument
                ("Condition",
                 Syn.Named_Subtype ("Selection_Condition'Class")),
            Result_Type => Table.Handle_Name,
            Block       => Block));
      Table_Package.Add_Separator;

   end Create_First_Where_Function;

   ---------------------
   -- Create_Iterator --
   ---------------------

   procedure Create_Iterator
     (Table_Package : in out Syn.Declarations.Package_Type'Class)
   is

      use Syn.Declarations, Syn.Types;

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
              ("Result.Position",
               Syn.Expressions.New_Function_Call_Expression
                 ("Element_Lists." & Name,
                  "Position.Position")));

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
              ("Position.Position",
               Syn.Object ("It.Container.Data_Source." & Name)));

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
         Component_Type => "Selection",
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

   ------------------------------------
   -- Create_Key_Condition_Functions --
   ------------------------------------

   procedure Create_Key_Condition_Functions
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is

      procedure Add_Key_Function
        (Base : Kit.Schema.Tables.Table_Type;
         Key  : Kit.Schema.Keys.Key_Type);

      ----------------------
      -- Add_Key_Function --
      ----------------------

      procedure Add_Key_Function
        (Base : Kit.Schema.Tables.Table_Type;
         Key  : Kit.Schema.Keys.Key_Type)
      is
         pragma Unreferenced (Base);

         Block   : Syn.Blocks.Block_Type;
         Ret_Seq : Syn.Statements.Sequence_Of_Statements;

      begin
         if Ignore_Key (Key) then
            return;
         end if;

         Block.Add_Declaration
           (Syn.Declarations.New_Object_Declaration
              ("Main_Key",
               Syn.Named_Subtype
                 ("Selection_Key (K_" & Key.Ada_Name & ")")));
         Block.Append
           (Syn.Statements.New_Assignment_Statement
              ("Main_Key.Has_Value", Syn.Object ("True")));

         declare
            procedure Add_Key_Field
              (Field : Kit.Schema.Fields.Field_Type;
               Name  : String);

            -------------------
            -- Add_Key_Field --
            -------------------

            procedure Add_Key_Field
              (Field : Kit.Schema.Fields.Field_Type;
               Name  : String)
            is
               Value : constant String :=
                 (if Field.Get_Field_Type.Is_Table_Reference
                  then Field.Ada_Name & ".Reference_"
                  & Field.Get_Field_Type.Ada_Name
                  else Field.Ada_Name);
            begin
               Field.Get_Field_Type.Set_Value
                 (Target_Name => "Main_Key." & Name,
                  Value_Name  => Value,
                  Sequence    => Block);
            end Add_Key_Field;

         begin
            Scan_Key_Fields (Key, Add_Key_Field'Access);
         end;

         Ret_Seq.Append
           (Syn.Statements.New_Assignment_Statement
              ("Result.Main_Key",
               Syn.Expressions.New_Function_Call_Expression
                 ("Selection_Key_Holders.To_Holder",
                  Syn.Object ("Main_Key"))));

         Block.Append
           (Syn.Statements.New_Return_Statement
              (Return_Variable   => "Result",
               Variable_Type     => "Selection_Condition",
               Return_Statements => Ret_Seq));

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => Key.Ada_Name,
                 Result_Type => "Selection_Condition",
                 Block       => Block);
         begin
            for I in 1 .. Key.Field_Count loop
               Fn.Add_Formal_Argument
                 (Syn.Declarations.New_Formal_Argument
                    (Name          => Key.Field (I).Ada_Name,
                     Argument_Type =>
                       Syn.Named_Subtype
                         (Key.Field (I)
                          .Get_Field_Type
                          .Argument_Handle_Subtype)));
            end loop;

            Table_Package.Append (Fn);
            Table_Package.Add_Separator;

         end;

      end Add_Key_Function;

   begin
      Table.Scan_Keys
        (Process           => Add_Key_Function'Access,
         Include_Base_Keys => True);
   end Create_Key_Condition_Functions;

   --------------------------------
   -- Create_Select_All_Function --
   --------------------------------

   procedure Create_Select_All_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      Seq_Ret   : Syn.Statements.Sequence_Of_Statements;
      Seq_Loop  : Syn.Statements.Sequence_Of_Statements;
      Block     : Syn.Blocks.Block_Type;
   begin
      Seq_Loop.Append
        (Syn.Statements.New_Procedure_Call_Statement
           (Procedure_Name => "Result.Data_Source.Append",
            Argument       =>
              Syn.Expressions.New_Function_Call_Expression
                (Procedure_Name => "Get",
                 Argument       =>
                   Syn.Object
                     ("Item.Get_" & Table.Ada_Name & "_Reference"))));

      Seq_Ret.Append
        (Syn.Statements.Iterate
           (Loop_Variable  => "Item",
            Container_Name =>
              Db.Database_Package_Name
            & "." & Table.Ada_Name
            & ".Scan_By_Top_Record",
            Iterate_Body   => Seq_Loop));

      Block.Append
        (Syn.Statements.New_Return_Statement
           ("Result", "Selection", Seq_Ret));

      Table_Package.Append
        (Syn.Declarations.New_Function
           (Name        => "Select_All",
            Result_Type => "Selection",
            Block       => Block));
      Table_Package.Add_Separator;

   end Create_Select_All_Function;

   ----------------------------------
   -- Create_Select_Where_Function --
   ----------------------------------

   procedure Create_Select_Where_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      Seq_Ret   : Syn.Statements.Sequence_Of_Statements;
      Seq_Loop  : Syn.Statements.Sequence_Of_Statements;
      Block     : Syn.Blocks.Block_Type;
   begin
      Seq_Loop.Append
        (Syn.Statements.If_Statement
           (Syn.Expressions.New_Function_Call_Expression
                ("Check_Constraints",
                 Syn.Object ("Condition.Constraints"),
                 Syn.Object ("Item")),
            Syn.Statements.New_Procedure_Call_Statement
              (Procedure_Name => "Result.Data_Source.Append",
               Argument       =>
                 Syn.Expressions.New_Function_Call_Expression
                   (Procedure_Name => "Get",
                    Argument       =>
                      Syn.Object
                        ("Item.Get_" & Table.Ada_Name & "_Reference")))));

      Seq_Ret.Append
        (Syn.Statements.Iterate
           (Loop_Variable  => "Item",
            Container_Name =>
              "Create_Selection ("
            & "Condition.Main_Key.Element"
            & ")",
            Iterate_Body   => Seq_Loop));

      Block.Append
        (Syn.Statements.New_Return_Statement
           ("Result", "Selection", Seq_Ret));

      Table_Package.Append
        (Syn.Declarations.New_Function
           (Name        => "Select_Where",
            Argument    =>
              Syn.Declarations.New_Formal_Argument
                ("Condition",
                 Syn.Named_Subtype ("Selection_Condition'Class")),
            Result_Type => "Selection",
            Block       => Block));
      Table_Package.Add_Separator;

   end Create_Select_Where_Function;

   -------------------------------------
   -- Create_Selection_Condition_Type --
   -------------------------------------

   procedure Create_Selection_Condition_Type
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      pragma Unreferenced (Table);
      Condition : Syn.Types.Record_Type_Definition;
   begin
      Condition.Set_Tagged;
      Condition.Add_Component
        ("Main_Key",
         "Selection_Key_Holders.Holder");
      Condition.Add_Component
        ("Constraints",
         "Selection_Constraint_Lists.List");

      declare
         Condition_Type : constant Syn.Declarations.Type_Declaration :=
           Syn.Declarations.New_Private_Type_Declaration
             ("Selection_Condition", Condition);
      begin
         Table_Package.Append (Condition_Type);
         Table_Package.Add_Separator;
      end;

   end Create_Selection_Condition_Type;

   -------------------------------
   -- Create_Selection_Function --
   -------------------------------

   procedure Create_Selection_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      use Syn.Expressions;
      Choose : Syn.Statements.Case_Statement_Record'Class :=
        Syn.Statements.Case_Statement
          ("Key.Key_Type");

      function Context
        (Call : Syn.Expression'Class)
         return Syn.Expression'Class
      is (Syn.Expressions.Operator
          (".", Syn.Object (Db.Database_Package_Name),
           Operator (".", Syn.Object (Table.Ada_Name),
                     Call)));

      procedure Add_Key
        (Base : Kit.Schema.Tables.Table_Type;
         Key : Kit.Schema.Keys.Key_Type);

      -------------
      -- Add_Key --
      -------------

      procedure Add_Key
        (Base : Kit.Schema.Tables.Table_Type;
         Key  : Kit.Schema.Keys.Key_Type)
      is
         pragma Unreferenced (Base);
         function Return_Context
           (Call       : String;
            With_Value : Boolean)
            return Syn.Statement'Class;

         --------------------
         -- Return_Context --
         --------------------

         function Return_Context
           (Call       : String;
            With_Value : Boolean)
            return Syn.Statement'Class
         is
            Fn : Syn.Expressions.Function_Call_Expression'Class :=
              Syn.Expressions.New_Function_Call_Expression
                (Call);

            procedure Add_Key_Argument
              (Field : Kit.Schema.Fields.Field_Type;
               Name  : String);

            ----------------------
            -- Add_Key_Argument --
            ----------------------

            procedure Add_Key_Argument
              (Field : Kit.Schema.Fields.Field_Type;
               Name  : String)
            is
            begin
               Fn.Add_Actual_Argument
                 (Field.Get_Field_Type.Return_Value
                    ("Key." & Name));
            end Add_Key_Argument;

         begin
            if With_Value then
               Scan_Key_Fields (Key, Add_Key_Argument'Access);
            end if;
            return Syn.Statements.New_Return_Statement
              (Context (Fn));
         end Return_Context;

      begin
         if not Ignore_Key (Key) then
            Choose.Add_Case_Option
              ("K_" & Key.Ada_Name,
               Syn.Statements.If_Statement
                 (Syn.Object ("Key.Has_Value"),
                  Return_Context
                    ("Select_By_" & Key.Ada_Name, True),
                  Return_Context
                    ("Scan_By_" & Key.Ada_Name, False)));
         end if;
      end Add_Key;

      Block  : Syn.Blocks.Block_Type;

   begin
      Choose.Add_Case_Option
        ("K_No_Selection_Key",
         Syn.Statements.New_Return_Statement
           (Context (Syn.Object ("Scan_By_Top_Record"))));

      Table.Scan_Keys (Add_Key'Access);

      Block.Append (Choose);

      Table_Package.Append_To_Body
        (Syn.Declarations.New_Function
           (Name        => "Create_Selection",
            Argument    =>
              Syn.Declarations.New_Formal_Argument
                ("Key", Syn.Named_Subtype ("Selection_Key")),
            Result_Type =>
              Db.Database_Package_Name
            & "." & Table.Ada_Name
            & "." & "Selection",
            Block       => Block));

   end Create_Selection_Function;

   --------------------------------------
   -- Create_Selection_Key_Enumeration --
   --------------------------------------

   procedure Create_Selection_Key_Enumeration
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is

      function Key_Enumeration return Syn.Type_Definition'Class;

      ---------------------
      -- Key_Enumeration --
      ---------------------

      function Key_Enumeration return Syn.Type_Definition'Class is
         Key_Enum : Syn.Enumeration_Type_Definition;

         procedure Add_Key_Literal
           (Base : Kit.Schema.Tables.Table_Type;
            Key  : Kit.Schema.Keys.Key_Type);

         ---------------------
         -- Add_Key_Literal --
         ---------------------

         procedure Add_Key_Literal
           (Base : Kit.Schema.Tables.Table_Type;
            Key  : Kit.Schema.Keys.Key_Type)
         is
            pragma Unreferenced (Base);
         begin
            if not Ignore_Key (Key) then
               Key_Enum.New_Literal ("K_" & Key.Ada_Name);
            end if;
         end Add_Key_Literal;

      begin
         Key_Enum.New_Literal ("K_No_Selection_Key");
         Table.Scan_Keys
           (Process           => Add_Key_Literal'Access,
            Include_Base_Keys => True);
         return Key_Enum;
      end Key_Enumeration;

      Key_Type : Syn.Declarations.Type_Declaration'Class :=
        Syn.Declarations.New_Full_Type_Declaration
          ("Selection_Key_Type", Key_Enumeration);
   begin
      Key_Type.Set_Private_Spec;
      Table_Package.Append (Key_Type);
   end Create_Selection_Key_Enumeration;

   ---------------------------------
   -- Create_Selection_Key_Holder --
   ---------------------------------

   procedure Create_Selection_Key_Holder
     (Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      Holder_Package   : Syn.Declarations.Package_Type :=
        Syn.Declarations.New_Package_Type
          ("Selection_Key_Holders");
   begin
      Holder_Package.Set_Generic_Instantiation
        ("Ada.Containers.Indefinite_Holders");
      Holder_Package.Add_Generic_Actual_Argument ("Selection_Key");
      Holder_Package.Set_Private_Spec;
      Table_Package.Append (Holder_Package);
   end Create_Selection_Key_Holder;

   -------------------------------
   -- Create_Selection_Key_Type --
   -------------------------------

   procedure Create_Selection_Key_Type
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      function Selection_Key_Record return Syn.Type_Definition'Class;

      --------------------------
      -- Selection_Key_Record --
      --------------------------

      function Selection_Key_Record return Syn.Type_Definition'Class is

         Key_Rec : Syn.Types.Record_Type_Definition;

         procedure Add_Key_Variant
           (Base : Kit.Schema.Tables.Table_Type;
            Key  : Kit.Schema.Keys.Key_Type);

         ---------------------
         -- Add_Key_Variant --
         ---------------------

         procedure Add_Key_Variant
           (Base : Kit.Schema.Tables.Table_Type;
            Key  : Kit.Schema.Keys.Key_Type)
         is
            pragma Unreferenced (Base);

            procedure Add_Component
              (Field : Kit.Schema.Fields.Field_Type;
               Name  : String);

            -------------------
            -- Add_Component --
            -------------------

            procedure Add_Component
              (Field : Kit.Schema.Fields.Field_Type;
               Name  : String)
            is
            begin
               Key_Rec.Add_Component
                 (Name, Field.Get_Field_Type.Record_Subtype);
            end Add_Component;

         begin
            if not Ignore_Key (Key) then
               Key_Rec.Next_Case_Option ("K_" & Key.Ada_Name);
               Scan_Key_Fields (Key, Add_Component'Access);
            end if;
         end Add_Key_Variant;

      begin
         Key_Rec.Add_Variant ("Key_Type", "Selection_Key_Type");
         Key_Rec.Add_Component ("Has_Value", "Boolean");
         Key_Rec.Start_Case ("K_No_Selection_Key");

         Table.Scan_Keys
           (Process           => Add_Key_Variant'Access,
            Include_Base_Keys => True);

         return Key_Rec;
      end Selection_Key_Record;

      Key_Type : Syn.Declarations.Type_Declaration'Class :=
        Syn.Declarations.New_Full_Type_Declaration
          ("Selection_Key", Selection_Key_Record);
   begin
      Key_Type.Set_Private_Spec;
      Table_Package.Append (Key_Type);
   end Create_Selection_Key_Type;

   ---------------------------
   -- Create_Selection_Type --
   ---------------------------

   procedure Create_Selection_Type
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
   is
      use Syn;
      use Syn.Declarations;
      use Syn.Expressions;
      use Syn.Statements;

      Selection          : Syn.Types.Record_Type_Definition;
      Cursor             : Syn.Types.Record_Type_Definition;
      Constant_Reference : Syn.Types.Record_Type_Definition;
      Iterator_Package   : Syn.Declarations.Package_Type :=
        Syn.Declarations.New_Package_Type
          ("Selection_Iterator_Interfaces");
   begin

      declare
         Name         : constant String := "Element_Lists";
         Type_Name    : constant String := Table.Handle_Name;
         List_Package : Syn.Declarations.Package_Type :=
           Syn.Declarations.New_Package_Type (Name);
      begin
         List_Package.Set_Generic_Instantiation
           ("Ada.Containers.Doubly_Linked_Lists");
         List_Package.Add_Generic_Actual_Argument (Type_Name);
         List_Package.Set_Private_Spec;
         Table_Package.Append (List_Package);
      end;

      Cursor.Add_Component
        ("Position", "Element_Lists.Cursor");

      Table_Package.Append
        (Syn.Declarations.New_Private_Type_Declaration
           ("Cursor", Cursor));

      Table_Package.Add_Separator;

      declare
         Has_Element_Block : Syn.Blocks.Block_Type;
      begin
         Has_Element_Block.Add_Statement
           (New_Return_Statement
              (New_Function_Call_Expression
                   ("Element_Lists.Has_Element",
                    Syn.Object ("Item.Position"))));
         Table_Package.Append
           (Syn.Declarations.New_Function
              (Name        => "Has_Element",
               Argument    =>
                 Syn.Declarations.New_Formal_Argument
                   ("Item", Syn.Named_Subtype ("Cursor")),
               Result_Type => "Boolean",
               Block       => Has_Element_Block));
      end;

      Table_Package.Add_Separator;

      Constant_Reference.Add_Variant
        ("Element",
         "not null access constant " & Table.Handle_Name);

      declare
         Ref_Type : Syn.Declaration'Class :=
           Syn.Declarations.New_Private_Type_Declaration
             ("Constant_Reference_Type", Constant_Reference);
      begin
         Ref_Type.Add_Aspect ("Implicit_Dereference",
                              Syn.Object ("Element"));
         Table_Package.Append (Ref_Type);
         Table_Package.Add_Separator;
      end;

      Selection.Set_Tagged;
      Selection.Set_Limited;
      Selection.Add_Component
        ("Data_Source",
         "Element_Lists.List");

      Iterator_Package.Set_Generic_Instantiation
        ("Ada.Iterator_Interfaces");
      Iterator_Package.Add_Generic_Actual_Argument ("Cursor");
      Iterator_Package.Add_Generic_Actual_Argument ("Has_Element");
      Table_Package.Append (Iterator_Package);
      Table_Package.Add_Separator;

      declare
         Selection_Type : Syn.Declarations.Type_Declaration :=
           Syn.Declarations.New_Private_Type_Declaration
             ("Selection", Selection);
      begin
         Selection_Type.Add_Aspect ("Constant_Indexing",
                                    "Constant_Reference");
         Selection_Type.Add_Aspect ("Default_Iterator",
                                    "Iterate");
         Selection_Type.Add_Aspect ("Iterator_Element",
                                    Table.Ada_Name & "_Handle");

         Table_Package.Append (Selection_Type);
      end;

      declare
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
           (Syn.Declarations.New_Function
              (Name        => "Iterate",
               Argument    =>
                 New_In_Argument
                   ("Container",
                    Named_Subtype ("Selection")),
               Result_Type =>
                 "Selection_Iterator_Interfaces.Reversible_Iterator'Class",
               Block       => Iterate_Block));
         Table_Package.Add_Separator;
      end;

      declare
         Ref_Block : Syn.Blocks.Block_Type;
      begin
         Ref_Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("Ref",
               "Element_Lists.Constant_Reference_Type",
               Syn.Expressions.New_Function_Call_Expression
                 ("Container.Data_Source.Constant_Reference",
                  "Position.Position")));

         Ref_Block.Add_Statement
           (Syn.Statements.New_Return_Statement
              (Result =>
                   Object ("(Element => Ref.Element)")));

         declare
            Fn_Name : constant String := "Constant_Reference";
            Ref_Fn    : Subprogram_Declaration'Class :=
              New_Function
                (Fn_Name,
                 Named_Subtype ("Constant_Reference_Type"),
                 Ref_Block);
            Container : Formal_Argument'Class :=
              New_Formal_Argument
                ("Container",
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
            Table_Package.Add_Separator;
         end;
      end;

      declare
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
                        ("Container.Data_Source.Is_Empty"))));
      begin
         Table_Package.Append (Is_Empty);
         Table_Package.Add_Separator;
      end;

      declare
         Element : constant Subprogram_Declaration'Class :=
           New_Function
             (Name        => "Element",
              Argument    =>
                New_In_Argument
                  (Name          => "Item",
                   Argument_Type =>
                     Named_Subtype ("Cursor")),
              Result_Type => Table.Handle_Name,
              Block       =>
                Syn.Blocks.Create_Block
                  (New_Return_Statement
                     (Object
                        ("Element_Lists.Element "
                         & "(Item.Position)")
                     )
                  )
             );
      begin
         Table_Package.Append (Element);
         Table_Package.Add_Separator;
      end;

   end Create_Selection_Type;

   --------------------------------
   -- Generate_Selection_Package --
   --------------------------------

   function Generate_Selection_Package
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : Syn.Declarations.Package_Type'Class)
      return Syn.Declarations.Package_Type'Class
   is
      Selections_Package : Syn.Declarations.Package_Type'Class :=
        Top.New_Child_Package ("Selections");

   begin

      Selections_Package.With_Package
        (Withed       => "Ada.Containers.Doubly_Linked_Lists",
         Private_With => True);

      Selections_Package.With_Package
        (Withed       => "Ada.Containers.Indefinite_Doubly_Linked_Lists",
         Private_With => True);

      Selections_Package.With_Package
        (Withed        => "Ada.Containers.Indefinite_Holders",
         Private_With  => True);

      Selections_Package.With_Package
        (Withed     => Db.Database_Package_Name,
         Body_With  => True);

      if Table.Has_String_Type then
         Selections_Package.With_Package
           ("Kit.Strings", Private_With => True);
      end if;

      Selections_Package.With_Package
        (Withed       => "Ada.Iterator_Interfaces");

      Create_Iterator (Selections_Package);
      Create_Selection_Type (Table, Selections_Package);
      Create_Selection_Key_Enumeration (Table, Selections_Package);
      Create_Selection_Key_Type (Table, Selections_Package);
      Create_Selection_Key_Holder (Selections_Package);

      Create_Constraint_Field_Enumeration (Table, Selections_Package);
      Create_Constraint_Field_Type (Table, Selections_Package);
      Create_Constraint_Field_List (Selections_Package);

      Create_Constraint_Check (Db, Table, Selections_Package);

      Create_Select_All_Function (Db, Table, Selections_Package);

      Create_Selection_Condition_Type (Table, Selections_Package);

      Create_Key_Condition_Functions (Table, Selections_Package);

      Create_Condition_Merge_Functions (Table, Selections_Package);

      Create_Field_Singleton_Types (Table, Selections_Package);

      Create_Selection_Function (Db, Table, Selections_Package);

      Create_First_Where_Function (Table, Selections_Package);
      Create_Select_Where_Function (Table, Selections_Package);

      return Selections_Package;

   end Generate_Selection_Package;

   -----------------------
   -- Operator_Function --
   -----------------------

   function Operator_Function
     (Operator : Kit.Schema.Types.Kit_Operator)
      return String
   is
      use all type Kit.Schema.Types.Kit_Operator;
   begin
      case Operator is
         when Is_False =>
            return "not";
         when Is_True =>
            return "";
         when EQ =>
            return """=""";
         when NE =>
            return """/=""";
         when LE =>
            return """<=""";
         when GE =>
            return """>=""";
         when LT =>
            return """<""";
         when GT =>
            return """>""";
      end case;
   end Operator_Function;

   ---------------------
   -- Scan_Key_Fields --
   ---------------------

   procedure Scan_Key_Fields
     (Key : Kit.Schema.Keys.Key_Type;
      Process : not null access
        procedure (Field : Kit.Schema.Fields.Field_Type;
                   Value_Name : String))
   is
   begin
      for I in 1 .. Key.Field_Count loop
         declare
            Field : constant Kit.Schema.Fields.Field_Type :=
              Key.Field (I);
            Name  : constant String :=
              (if Key.Field_Count = 1
               then Key.Ada_Name & "_Value"
               else Key.Ada_Name & "_" & Field.Ada_Name);
         begin
            Process (Field, Name);
         end;
      end loop;
   end Scan_Key_Fields;

end Kit.Generate.Selections;
