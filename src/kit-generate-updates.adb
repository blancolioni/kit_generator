with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;

with Kit.Schema.Fields;
with Kit.Schema.Types;

package body Kit.Generate.Updates is

   procedure Create_Update_Start
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Update_Finish
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Field_Update_Function
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class;
      Field  : Kit.Schema.Fields.Field_Type);

   ----------------------------------
   -- Create_Field_Update_Function --
   ----------------------------------

   procedure Create_Field_Update_Function
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class;
      Field  : Kit.Schema.Fields.Field_Type)
   is
   begin
      if Field.Writeable then
         declare
            Block : Syn.Blocks.Block_Type;
            R_Seq : Syn.Statements.Sequence_Of_Statements;
         begin
            Block.Add_Declaration
              (Syn.Declarations.New_Object_Declaration
                 ("Change",
                  Table.Ada_Name & "_Update_Value ("
                  & "Update_" & Field.Ada_Name & ")"));

            if Field.Get_Field_Type.Is_Text then
               Block.Append
                 (Syn.Statements.New_Assignment_Statement
                    ("Change." & Field.Ada_Name & "_Value",
                     Syn.Expressions.New_Function_Call_Expression
                       ("Ada.Strings.Unbounded.To_Unbounded_String",
                        "Value")));
            elsif Field.Get_Field_Type.Is_Table_Reference then
               Field.Get_Field_Type.Set_Value
                 (Target_Name => "Change." & Field.Ada_Name & "_Value",
                  Value_Name  =>
                    "Value.Reference_"
                  & Field.Get_Field_Type.Ada_Name,
                  Sequence    => Block);
            else
               Field.Get_Field_Type.Set_Value
                 (Target_Name => "Change." & Field.Ada_Name & "_Value",
                  Value_Name  => "Value",
                  Sequence    => Block);
            end if;

            R_Seq.Append
              (Syn.Statements.New_Assignment_Statement
                 ("Result.Reference", Syn.Object ("Update.Reference")));
            R_Seq.Append
              (Syn.Statements.New_Assignment_Statement
                 ("Result.Updates", Syn.Object ("Update.Updates")));

            R_Seq.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Result.Updates.Append", Syn.Object ("Change")));
            Block.Append
              (Syn.Statements.New_Return_Statement
                 ("Result", Table.Ada_Name & "_Update_Handle", R_Seq));

            declare
               Argument_Type : constant Kit.Schema.Types.Kit_Type :=
                                 Field.Get_Field_Type;
               Argument_Type_Name : constant String :=
                 (if Argument_Type.Is_Table_Reference
                  then Db.Handle_Package_Name
                  & "." & Argument_Type.Ada_Name
                  & "." & Argument_Type.Ada_Name & "_Class"
                  elsif Argument_Type.Has_Custom_Type
                  then Db.Database_Package_Name
                  & "." & Argument_Type.Ada_Name
                  else Argument_Type.Argument_Subtype);
            begin
               Target.Append
                 (Syn.Declarations.New_Function
                    ("Set_" & Field.Ada_Name,
                     Syn.Declarations.New_Formal_Argument
                       ("Update",
                        Syn.Named_Subtype (Table.Ada_Name & "_Update_Handle")),
                     Syn.Declarations.New_Formal_Argument
                       ("Value",
                        Syn.Named_Subtype (Argument_Type_Name)),
                     Table.Ada_Name & "_Update_Handle",
                     Block));
            end;
         end;

      end if;
   end Create_Field_Update_Function;

   --------------------------
   -- Create_Update_Finish --
   --------------------------

   procedure Create_Update_Finish
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class)
   is
      Field_Case : Syn.Statements.Case_Statement_Record'Class :=
        Syn.Statements.Case_Statement ("Item.Field");

      procedure Add_Update
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      ----------------
      -- Add_Update --
      ----------------

      procedure Add_Update
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
         Sequence : Syn.Statements.Sequence_Of_Statements;
      begin
         if Field.Writeable then
            if Field.Get_Field_Type.Is_Text then
               Sequence.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Rec.Set_" & Field.Ada_Name,
                     Syn.Expressions.New_Function_Call_Expression
                       ("Ada.Strings.Unbounded.To_String",
                        "Item." & Field.Ada_Name & "_Value")));
            else
               Sequence.Append
                 (Syn.Statements.New_Procedure_Call_Statement
                    ("Rec.Set_" & Field.Ada_Name,
                     Field.Get_Field_Type.Return_Value
                       ("Item." & Field.Ada_Name & "_Value")));
            end if;
            Field_Case.Add_Case_Option
              (Value => "Update_" & Field.Ada_Name,
               Stats => Sequence);
         end if;
      end Add_Update;

      Update_Sequence : Syn.Statements.Sequence_Of_Statements;

   begin

      Table.Iterate_All (Add_Update'Access);

      Update_Sequence.Append (Field_Case);

      declare
         Iterator : constant Syn.Statement'Class :=
           Syn.Statements.Iterate
             (Loop_Variable  => "Item",
              Container_Name => "Update.Updates",
              Iterate_Body   => Update_Sequence);
         Block    : Syn.Blocks.Block_Type;
         Prefix   : constant String :=
                      Db.Database_Package_Name
                      & "."
                      & Table.Ada_Name
                      & ".";
         Update_Type_Name : constant String :=
                              Prefix & Table.Update_Type_Name;
         Get_Update_Name  : constant String :=
                              Prefix & "Get_Update";
      begin
         Block.Add_Declaration
           (Syn.Declarations.New_Object_Declaration
              ("Rec",
               Syn.Named_Subtype (Update_Type_Name),
               Syn.Expressions.New_Function_Call_Expression
                 (Get_Update_Name, "Update.Reference")));

         Block.Append (Iterator);
         Block.Append
           (Syn.Statements.New_Procedure_Call_Statement
              ("Cache.Invalidate",
               Syn.Object ("Update.Reference")));

         Target.Append
           (Syn.Declarations.New_Procedure
              ("Done",
               Syn.Declarations.New_Formal_Argument
                 ("Update",
                  Syn.Named_Subtype (Table.Ada_Name & "_Update_Handle")),
               Block));
      end;

   end Create_Update_Finish;

   -------------------------
   -- Create_Update_Start --
   -------------------------

   procedure Create_Update_Start
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class)
   is
      Return_Sequence : Syn.Statements.Sequence_Of_Statements;
      Block           : Syn.Blocks.Block_Type;
   begin
      Return_Sequence.Append
        (Syn.Statements.New_Assignment_Statement
           (Target => "Update.Reference",
            Value  => Syn.Object ("Target")));
      Block.Append
        (Syn.Statements.New_Return_Statement
           ("Update", Table.Ada_Name & "_Update_Handle",
            Return_Sequence));

      Target.Append
        (Syn.Declarations.New_Function
           ("Update_" & Table.Ada_Name,
            Syn.Declarations.New_Formal_Argument
              ("Target",
               Syn.Named_Subtype
                 (Db.Database_Package_Name
                  & "."
                  & Table.Reference_Type_Name)),
            Table.Ada_Name & "_Update_Handle",
            Block));

   end Create_Update_Start;

   ------------------------
   -- Create_Update_Type --
   ------------------------

   procedure Create_Update_Type
     (Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class)
   is
      Update_Type       : Syn.Types.Record_Type_Definition;
      Element_Type      : Syn.Types.Record_Type_Definition;
      Field_Enumeration : Syn.Enumeration_Type_Definition;

      procedure Add_Field_Component
        (Table : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      -------------------------
      -- Add_Field_Component --
      -------------------------

      procedure Add_Field_Component
        (Table : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Table);
         Literal_Name : constant String :=
           "Update_" & Field.Ada_Name;
         Component_Name : constant String :=
           Field.Ada_Name & "_Value";
      begin

         if Field.Writeable then
            Field_Enumeration.New_Literal (Literal_Name);
            Element_Type.Next_Case_Option (Literal_Name);

            if Field.Get_Field_Type.Is_Text then
               Element_Type.Add_Component
                 (Component_Name => Component_Name,
                  Component_Type => "Ada.Strings.Unbounded.Unbounded_String");
            else
               Element_Type.Add_Component
                 (Component_Name => Component_Name,
                  Component_Type => Field.Get_Field_Type.Record_Subtype);
            end if;
         end if;
      end Add_Field_Component;

      Fields_Type       : constant String :=
        Table.Ada_Name & "_Fields";
      Update_Value      : constant String :=
        Table.Ada_Name & "_Update_Value";

   begin

      Target.With_Package
        ("Ada.Containers.Indefinite_Doubly_Linked_Lists",
         Private_With => True);

      if Table.Has_String_Type then
         Target.With_Package
           ("Kit.Strings",
            Private_With => True);
      end if;

      Element_Type.Add_Variant
        (Variant_Name    => "Field",
         Variant_Type    => Fields_Type);
      Table.Iterate_All (Add_Field_Component'Access);

      Update_Type.Set_Tagged;
      Update_Type.Set_Limited;
      Update_Type.Add_Component
        ("Reference", Table.Reference_Type_Name);
      Update_Type.Add_Component
        ("Updates", "Update_Lists.List");

      declare
         Field_Enum_Type : Syn.Declarations.Type_Declaration :=
           Syn.Declarations.New_Full_Type_Declaration
             (Fields_Type, Field_Enumeration);
      begin
         Field_Enum_Type.Set_Private_Spec;
         Target.Append (Field_Enum_Type);
      end;

      declare
         Element_Record_Type : Syn.Declarations.Type_Declaration :=
           Syn.Declarations.New_Full_Type_Declaration
             (Update_Value, Element_Type);
      begin
         Element_Record_Type.Set_Private_Spec;
         Target.Append (Element_Record_Type);
      end;

      declare
         Update_Lists : Syn.Declarations.Package_Type :=
           Syn.Declarations.New_Package_Type
             ("Update_Lists");
      begin
         Update_Lists.Set_Generic_Instantiation
           ("Ada.Containers.Indefinite_Doubly_Linked_Lists");
         Update_Lists.Add_Generic_Actual_Argument (Update_Value);
         Update_Lists.Set_Private_Spec;
         Target.Append (Update_Lists);
      end;

      declare
         Update : constant Syn.Declarations.Type_Declaration :=
           Syn.Declarations.New_Private_Type_Declaration
             (Identifier => Table.Ada_Name & "_Update_Handle",
              Definition => Update_Type);
      begin
         Target.Append (Update);
      end;

   end Create_Update_Type;

   ---------------------------------
   -- Generate_Update_Subprograms --
   ---------------------------------

   procedure Generate_Update_Subprograms
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class)
   is
   begin

      Create_Update_Start (Db, Table, Target);
      Create_Update_Finish (Db, Table, Target);

      declare
         procedure Create_Function
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type);

         ---------------------
         -- Create_Function --
         ---------------------

         procedure Create_Function
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type)
         is
            pragma Unreferenced (Base);
         begin
            if Field.Writeable then
               Create_Field_Update_Function (Db, Table, Target, Field);
            end if;
         end Create_Function;

      begin
         Table.Iterate_All (Create_Function'Access);
      end;

   end Generate_Update_Subprograms;

end Kit.Generate.Updates;
