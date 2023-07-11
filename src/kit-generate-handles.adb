with Ada.Containers.Indefinite_Vectors;

with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;

with Kit.Schema.Fields;
with Kit.Schema.Keys;
with Kit.Schema.Types;

with Kit.String_Maps;

with Kit.Generate.Updates;

with Kit.Generate.Handles.Search;

package body Kit.Generate.Handles is

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   function Find_Field_Type_Table_References
     (Table : Kit.Schema.Tables.Table_Type)
      return String_Vectors.Vector;

   function Find_Custom_Type_References
     (Table : Kit.Schema.Tables.Table_Type)
      return String_Vectors.Vector;

   procedure Create_Handle_Cache
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Handle_Type
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Key_Functions
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class);

   -------------------------
   -- Create_Handle_Cache --
   -------------------------

   procedure Create_Handle_Cache
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class)
   is

      Reference_Map_Package_Name : constant String := "Cached_Handle_Maps";
      Cached_Handle_Name : constant String := "Cached_Handle";

      function Is_Cached
        (Field : Kit.Schema.Fields.Field_Type)
         return Boolean
      is (True or else not Field.Base_Reference);

      procedure Create_Cached_Record;
      procedure Create_Cached_Map;
      procedure Create_Cache_Functions;

      ----------------------------
      -- Create_Cache_Functions --
      ----------------------------

      procedure Create_Cache_Functions is

         procedure Create_Load_Cached_Record;
         procedure Create_Get_Statistics;

         procedure Create_Get_Statistics is
            Block : Syn.Blocks.Block_Type;
         begin
            Block.Append
              (Syn.Statements.New_Procedure_Call_Statement
                 ("Cache.Get_Statistics",
                  Syn.Object ("Size"),
                  Syn.Object ("Hits"),
                  Syn.Object ("Misses")));

            Target.Append
              (Syn.Declarations.New_Procedure
                 (Name       => "Get_Cache_Statistics",
                  Argument_1 =>
                    Syn.Declarations.New_Formal_Argument
                      ("Size",
                       Syn.Declarations.Out_Argument,
                       Syn.Named_Subtype ("Natural")),
                  Argument_2 =>
                    Syn.Declarations.New_Formal_Argument
                      ("Hits",
                       Syn.Declarations.Out_Argument,
                       Syn.Named_Subtype ("Natural")),
                  Argument_3 =>
                    Syn.Declarations.New_Formal_Argument
                      ("Misses",
                       Syn.Declarations.Out_Argument,
                       Syn.Named_Subtype ("Natural")),
                  Block      => Block));
         end Create_Get_Statistics;

         -------------------------------
         -- Create_Load_Cached_Record --
         -------------------------------

         procedure Create_Load_Cached_Record is
            Block : Syn.Blocks.Block_Type;

            procedure Copy_Value
              (Base  : Kit.Schema.Tables.Table_Type;
               Field : Kit.Schema.Fields.Field_Type);

            ----------------
            -- Copy_Value --
            ----------------

            procedure Copy_Value
              (Base  : Kit.Schema.Tables.Table_Type;
               Field : Kit.Schema.Fields.Field_Type)
            is
               pragma Unreferenced (Base);
            begin
               if Is_Cached (Field) then
                  if Field.Base_Reference then
                     Field.Get_Field_Type.Set_Value
                       (Target_Name => "Cached.Kit_Base_" & Field.Ada_Name,
                        Value_Name  =>
                          "Rec.Get_" & Field.Ada_Name
                        & "_Reference",
                        Sequence    => Block);
                  elsif Field.Get_Field_Type.Is_Text then
                     Block.Append
                       (Syn.Statements.New_Assignment_Statement
                          (Target => "Cached." & Field.Ada_Name,
                           Value  =>
                             Syn.Expressions.New_Function_Call_Expression
                               ("Ada.Strings.Unbounded.To_Unbounded_String",
                                "Rec." & Field.Ada_Name)));
                  else
                     Field.Get_Field_Type.Set_Value
                       (Target_Name => "Cached." & Field.Ada_Name,
                        Value_Name  => "Rec." & Field.Ada_Name,
                        Sequence    => Block);
                  end if;
               end if;
            end Copy_Value;

         begin
            Block.Add_Declaration
              (Syn.Declarations.New_Constant_Declaration
                 ("Rec",
                  Db.Database_Package_Name
                  & "." & Table.Ada_Name
                  & "." & Table.Type_Name,
                  Syn.Expressions.New_Function_Call_Expression
                    (Db.Database_Package_Name
                     & "." & Table.Ada_Name
                     & "." & "Get",
                     Syn.Object ("Reference"))));
            Table.Iterate_All (Copy_Value'Access);

            Target.Append_To_Body
              (Syn.Declarations.New_Procedure
                 (Name       => "Load",
                  Argument_1 =>
                    Syn.Declarations.New_Formal_Argument
                      ("Reference",
                       Syn.Named_Subtype
                         (Db.Database_Package_Name
                          & "." & Table.Ada_Name
                          & "_Reference")),
                  Argument_2 =>
                    Syn.Declarations.New_Formal_Argument
                      ("Cached", Syn.Declarations.Inout_Argument,
                       Syn.Named_Subtype (Cached_Handle_Name)),
                  Block      => Block));
         end Create_Load_Cached_Record;

      begin
         Create_Load_Cached_Record;
         Create_Get_Statistics;
      end Create_Cache_Functions;

      -----------------------
      -- Create_Cached_Map --
      -----------------------

      procedure Create_Cached_Map is
         Map_Package   : Syn.Declarations.Package_Type :=
           Syn.Declarations.New_Package_Type
             (Reference_Map_Package_Name);
      begin
         Map_Package.Set_Generic_Instantiation
           ("Kit.Protected_Maps");
         Map_Package.Add_Generic_Actual_Argument
           (Db.Database_Package_Name & "."
              & Table.Reference_Type_Name);
         Map_Package.Add_Generic_Actual_Argument
           (Cached_Handle_Name);
         Map_Package.Add_Generic_Actual_Argument
           ("Load");
         Map_Package.Add_Generic_Actual_Argument
           (Db.Database_Package_Name
            & "." & Table.Ada_Name & "_Hashes.Hash");
         Map_Package.Add_Generic_Actual_Argument
           ("Db.""=""");
         Target.Append_To_Body (Map_Package);
      end Create_Cached_Map;

      --------------------------
      -- Create_Cached_Record --
      --------------------------

      procedure Create_Cached_Record is
         Definition : Syn.Types.Record_Type_Definition;

         procedure Add_Component
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type);

         -------------------
         -- Add_Component --
         -------------------

         procedure Add_Component
           (Base  : Kit.Schema.Tables.Table_Type;
            Field : Kit.Schema.Fields.Field_Type)
         is
            pragma Unreferenced (Base);
            Component_Name : constant String :=
              (if Field.Base_Reference
               then "Kit_Base_" & Field.Ada_Name
               else Field.Ada_Name);
         begin
            if Is_Cached (Field) then
               if Field.Get_Field_Type.Is_Text then
                  Definition.Add_Component
                    (Component_Name,
                     "Ada.Strings.Unbounded.Unbounded_String");
               else
                  Definition.Add_Component
                    (Component_Name,
                     Field.Get_Field_Type.Record_Subtype);
               end if;
            end if;
         end Add_Component;

      begin
         Table.Iterate_All
           (Add_Component'Access);

         Target.Append_To_Body
           (Syn.Declarations.New_Full_Type_Declaration
              (Identifier => Cached_Handle_Name,
               Definition => Definition));
      end Create_Cached_Record;

   begin
      Create_Cached_Record;
      Create_Cache_Functions;
      Create_Cached_Map;

      Target.Append_To_Body
        (Syn.Declarations.New_Subtype_Declaration
           ("Constant_Reference_Type",
            Syn.Named_Subtype
              (Reference_Map_Package_Name
               & "." & "Constant_Reference_Type")));

      Target.Append_To_Body
        (Syn.Declarations.New_Object_Declaration
           ("Cache", Reference_Map_Package_Name & ".Map"));

   end Create_Handle_Cache;

   ------------------------
   -- Create_Handle_Type --
   ------------------------

   procedure Create_Handle_Type
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class)
   is
      Interface_Name : constant String :=
        Table.Ada_Name & "_Interface";
      Class_Name     : constant String :=
        Table.Ada_Name & "_Class";
      Handle_Name    : constant String :=
        Table.Ada_Name & "_Handle";

      Interface_Definition : Syn.Interface_Type_Definition;
      Handle_Definition    : Syn.Types.Record_Type_Definition;

      Interface_Argument : constant Syn.Declarations.Formal_Argument'Class :=
        Syn.Declarations.New_Formal_Argument
          (Name          => "Handle",
           Argument_Type => Syn.Named_Subtype (Interface_Name));

      Handle_Argument : constant Syn.Declarations.Formal_Argument'Class :=
        Syn.Declarations.New_Formal_Argument
          (Name          => "Handle",
           Argument_Type => Syn.Named_Subtype (Handle_Name));

      Withed_Tables    : Kit.String_Maps.String_Map;
      Withed_Db_Tables : Kit.String_Maps.String_Map;

      procedure Add_Base
        (Base : Kit.Schema.Tables.Table_Type);

      procedure Add_Base_Db
        (Base : Kit.Schema.Tables.Table_Type);

      procedure Add_Field_Type_With
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      procedure Create_Abstract_Property
        (Field : Kit.Schema.Fields.Field_Type);

      procedure Create_Abstract_Reference_Functions;

      procedure Create_Property
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      procedure Create_Get_Functions;

      procedure Create_Reference_Functions;
      procedure Create_Update_Functions;

      procedure Create_Null_Handle_Function;
      procedure Create_New_Record_Function;

      procedure Create_Base_Conversion
        (Base : Kit.Schema.Tables.Table_Type);

      procedure Create_From_Class_Conversion;

      function Return_Type_Name
        (Field : Kit.Schema.Fields.Field_Type)
         return String;

      --------------
      -- Add_Base --
      --------------

      procedure Add_Base
        (Base : Kit.Schema.Tables.Table_Type)
      is
      begin
         if not Withed_Tables.Contains (Base.Ada_Name) then
            Target.With_Package
              (Db.Handle_Package_Name & "." & Base.Package_Name);
            Withed_Tables.Insert (Base.Ada_Name);
         end if;

         Interface_Definition.Add_Parent
           (Name => Base.Package_Name & "." & Base.Ada_Name & "_Interface");
      end Add_Base;

      -----------------
      -- Add_Base_Db --
      -----------------

      procedure Add_Base_Db
        (Base : Kit.Schema.Tables.Table_Type)
      is
      begin
         if not Withed_Db_Tables.Contains (Base.Ada_Name) then
            Target.With_Package
              (Db.Database_Package_Name
               & "." & Base.Package_Name,
              Body_With => True);
            Withed_Db_Tables.Insert (Base.Ada_Name);
         end if;
      end Add_Base_Db;

      -------------------------
      -- Add_Field_Type_With --
      -------------------------

      procedure Add_Field_Type_With
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
      begin
         if Field.Get_Field_Type.Is_Table_Reference then
            declare
               Table_Name : constant String :=
                 Field.Get_Field_Type.Referenced_Table_Name;
            begin
               if not Withed_Tables.Contains (Table_Name)
                 and then Table_Name /= Table.Ada_Name
               then
                  Target.With_Package
                    (Db.Handle_Package_Name & "." & Table_Name);
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
                  Target.With_Package
                    (Type_Package);
                  Withed_Tables.Insert (Type_Package);
               end if;
            end;
         end if;
      end Add_Field_Type_With;

      ------------------------------
      -- Create_Abstract_Property --
      ------------------------------

      procedure Create_Abstract_Property
        (Field : Kit.Schema.Fields.Field_Type)
      is
      begin
         if not Field.Base_Reference then
            Target.Append
              (Syn.Declarations.New_Abstract_Function
                 (Name        => Field.Ada_Name,
                  Argument    => Interface_Argument,
                  Result_Type =>
                    Syn.Named_Subtype (Return_Type_Name (Field))));
         end if;
      end Create_Abstract_Property;

      -----------------------------------------
      -- Create_Abstract_Reference_Functions --
      -----------------------------------------

      procedure Create_Abstract_Reference_Functions is
      begin
         Target.Append
           (Syn.Declarations.New_Abstract_Function
              (Name        => "Reference_" & Table.Ada_Name,
               Argument    => Interface_Argument,
               Result_Type =>
                 Syn.Named_Subtype
                   (Db.Database_Package_Name
                    & "." & Table.Ada_Name & "_Reference")));

         if Table.Has_Writable_Field then
            Target.Append
              (Syn.Declarations.New_Abstract_Function
                 (Name        => "Update_" & Table.Ada_Name,
                  Argument    => Interface_Argument,
                  Result_Type =>
                    Syn.Named_Subtype
                      (Table.Ada_Name & "_Update_Handle'Class")));
         end if;
      end Create_Abstract_Reference_Functions;

      ----------------------------
      -- Create_Base_Conversion --
      ----------------------------

      procedure Create_Base_Conversion
        (Base : Kit.Schema.Tables.Table_Type)
      is
         Block       : Syn.Blocks.Block_Type;
         Base_Handle : constant String :=
           Db.Handle_Package_Name
           & "." & Base.Ada_Name
           & "." & Base.Ada_Name & "_Handle";
      begin

         Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("Rec",
               Db.Database_Package_Name & "."
               & Table.Ada_Name & "."
               & Table.Ada_Name & "_Type",
               Syn.Expressions.New_Function_Call_Expression
                 (Db.Database_Package_Name
                  & "."
                  & Table.Ada_Name
                  & "."
                  & "Get",
                  "Handle.Reference")));

         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.New_Function_Call_Expression
                   (Db.Handle_Package_Name
                    & "." & Base.Ada_Name
                    & ".Get",
                    Syn.Object ("Rec.Get_" & Base.Ada_Name & "_Reference"))));

         declare
            Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => Base.Ada_Name & "_Handle",
                 Argument    => Handle_Argument,
                 Result_Type => Base_Handle,
                 Block       => Block);
         begin
            Target.Append (Fn);
         end;
      end Create_Base_Conversion;

      ----------------------------------
      -- Create_From_Class_Conversion --
      ----------------------------------

      procedure Create_From_Class_Conversion is
         Block       : Syn.Blocks.Block_Type;
      begin
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.New_Function_Call_Expression
                   ("Get",
                    Syn.Object ("Class.Reference_" & Table.Ada_Name))));
         declare
            Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
                   Syn.Declarations.New_Function
                     (Name        => "To_" & Table.Ada_Name & "_Handle",
                      Argument    =>
                        Syn.Declarations.New_Formal_Argument
                          ("Class",
                           Syn.Named_Subtype (Table.Ada_Name & "_Class")),
                         Result_Type => Table.Handle_Name,
                         Block       => Block);
         begin
            Target.Append (Fn);
         end;
      end Create_From_Class_Conversion;

      --------------------------
      -- Create_Get_Functions --
      --------------------------

      procedure Create_Get_Functions is
         Block : Syn.Blocks.Block_Type;
         Ret   : Syn.Statements.Sequence_Of_Statements;
      begin
         Ret.Append
           (Syn.Statements.New_Assignment_Statement
              (Target => "Handle.Reference",
               Value  => Syn.Object ("Reference")));
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Return_Variable   => "Handle",
               Variable_Type     => Table.Ada_Name & "_Handle",
               Return_Statements => Ret));

         declare
            Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Get",
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Reference",
                      Syn.Named_Subtype
                        (Db.Database_Package_Name
                         & "." & Table.Ada_Name & "_Reference")),
                 Result_Type => Table.Ada_Name & "_Handle",
                 Block       => Block);
         begin
            Target.Append (Fn);
         end;

      end Create_Get_Functions;

      --------------------------------
      -- Create_New_Record_Function --
      --------------------------------

      procedure Create_New_Record_Function is

         Block      : Syn.Blocks.Block_Type;
         Proc_Block : Syn.Blocks.Block_Type;

         Call : Syn.Expressions.Function_Call_Expression :=
           Syn.Expressions.New_Function_Call_Expression
             (Db.Database_Package_Name
              & "." & Table.Ada_Name & ".Create");

         procedure Add_Actual_Argument
           (Base     : Kit.Schema.Tables.Table_Type;
            Field    : Kit.Schema.Fields.Field_Type);

         procedure Add_Create_Arguments
           (Sub : in out Syn.Declarations.Subprogram_Declaration'Class);

         function Call_Create_Function
           return Syn.Expression'Class;

         -------------------------
         -- Add_Actual_Argument --
         -------------------------

         procedure Add_Actual_Argument
           (Base     : Kit.Schema.Tables.Table_Type;
            Field    : Kit.Schema.Fields.Field_Type)
         is
            pragma Unreferenced (Base);
            Argument_Type : constant Kit.Schema.Types.Kit_Type :=
              Field.Get_Field_Type;

            Argument_Value : constant String :=
              (if Argument_Type.Is_Table_Reference
               then Field.Ada_Name
               & ".Reference_" & Argument_Type.Ada_Name
               else Field.Ada_Name);
         begin
            if Field.Created then
               Call.Add_Actual_Argument
                 (Syn.Object (Argument_Value));
            end if;
         end Add_Actual_Argument;

         --------------------------
         -- Add_Create_Arguments --
         --------------------------

         procedure Add_Create_Arguments
           (Sub : in out Syn.Declarations.Subprogram_Declaration'Class)
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
               Default_Value      : constant Syn.Expression'Class :=
                                      (if Argument_Type.Is_Table_Reference
                                       then Syn.Object
                                         (Db.Handle_Package_Name
                                          & "."
                                          & Argument_Type.Ada_Name
                                          & ".Empty_Handle")
                                       else Argument_Type
                                       .Default_Argument_Value);
            begin
               if Field.Created then
                  if Field.Has_Default_Value then
                     Sub.Add_Formal_Argument
                       (Field.Ada_Name,
                        Argument_Type_Name,
                        Default_Value);
                  else
                     Sub.Add_Formal_Argument
                       (Field.Ada_Name,
                        Argument_Type_Name);
                  end if;
               end if;
            end Add_Formal_Argument;

         begin
            Table.Iterate_All (Add_Formal_Argument'Access);
         end Add_Create_Arguments;

         --------------------------
         -- Call_Create_Function --
         --------------------------

         function Call_Create_Function
           return Syn.Expression'Class
         is
            Call : Syn.Expressions.Function_Call_Expression'Class :=
              Syn.Expressions.New_Function_Call_Expression ("Create");

            procedure Add_Actual_Argument
              (Base     : Kit.Schema.Tables.Table_Type;
               Field    : Kit.Schema.Fields.Field_Type);

            -------------------------
            -- Add_Actual_Argument --
            -------------------------

            procedure Add_Actual_Argument
              (Base     : Kit.Schema.Tables.Table_Type;
               Field    : Kit.Schema.Fields.Field_Type)
            is
               pragma Unreferenced (Base);
            begin
               if Field.Created then
                  Call.Add_Actual_Argument
                    (Syn.Object (Field.Ada_Name));
               end if;
            end Add_Actual_Argument;

         begin
            Table.Iterate_All (Add_Actual_Argument'Access);
            return Call;
         end Call_Create_Function;

      begin
         Table.Iterate_All (Add_Actual_Argument'Access);

         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.New_Function_Call_Expression
                   ("Get",
                    Call)));

         declare
            Create : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                ("Create",
                 Syn.Named_Subtype
                   (Table.Ada_Name & "_Handle"),
                 Block);
         begin
            Add_Create_Arguments (Create);
            Target.Append (Create);
         end;

         Proc_Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("Handle", Table.Handle_Name,
               Call_Create_Function));

         Proc_Block.Add_Declaration
           (Syn.Declarations.New_Pragma
              ("Unreferenced", "Handle"));
         Proc_Block.Add_Statement
           (Syn.Statements.New_Null_Statement);

         declare
            Create : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Procedure
                ("Create",
                 Proc_Block);
         begin
            Add_Create_Arguments (Create);
            Target.Append (Create);
         end;

      end Create_New_Record_Function;

      ---------------------------------
      -- Create_Null_Handle_Function --
      ---------------------------------

      procedure Create_Null_Handle_Function is

         Block      : Syn.Blocks.Block_Type;

      begin
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.New_Function_Call_Expression
                   ("Get",
                    Db.Database_Package_Name
                    & "."
                    & "Null_" & Table.Ada_Name & "_Reference")));

         Target.Append
           (Syn.Declarations.New_Function
              ("Empty_Handle",
               Syn.Named_Subtype
                 (Table.Ada_Name & "_Handle"),
               Block));

         Target.Add_Separator;

      end Create_Null_Handle_Function;

      ---------------------
      -- Create_Property --
      ---------------------

      procedure Create_Property
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
         Block : Syn.Blocks.Block_Type;
         Function_Name : constant String :=
           (if Field.Base_Reference
            then "Reference_" & Field.Ada_Name
            else Field.Ada_Name);

         Return_Type : constant Kit.Schema.Types.Kit_Type :=
           Field.Get_Field_Type;
         Return_Name : constant String :=
           (if Field.Base_Reference
            then Db.Database_Package_Name & "."
            & Field.Ada_Name & "_Reference"
            else Return_Type_Name (Field));
      begin

         Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("Rec",
               "Constant_Reference_Type",
               Syn.Expressions.New_Function_Call_Expression
                 ("Cache.Constant_Reference",
                  Syn.Object ("Handle.Reference"))));

         declare
            Component_Name : constant String :=
              (if Field.Base_Reference
               then "Kit_Base_" & Field.Ada_Name
               else Field.Ada_Name);
            Fetch_Expr     : constant Syn.Expression'Class :=
              (if not Field.Base_Reference
               and then Return_Type.Is_Table_Reference
               then Syn.Expressions.New_Function_Call_Expression
                 (Db.Handle_Package_Name
                  & "." & Return_Type.Ada_Name
                  & ".Get",
                  Syn.Object ("Rec." & Component_Name))
               elsif Field.Get_Field_Type.Is_Text then
                  Syn.Expressions.New_Function_Call_Expression
                 ("Ada.Strings.Unbounded.To_String",
                  Syn.Object ("Rec." & Component_Name))
               else Return_Type.Return_Value
                 ("Rec." & Component_Name));
         begin
            Block.Add_Statement
              (Syn.Statements.New_Return_Statement
                 (Fetch_Expr));
         end;

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => Function_Name,
                 Argument    => Handle_Argument,
                 Result_Type => Return_Name,
                 Block       => Block);
         begin
            Fn.Set_Overriding;
            Target.Append (Fn);
         end;
      end Create_Property;

      --------------------------------
      -- Create_Reference_Functions --
      --------------------------------

      procedure Create_Reference_Functions is
         Block             : Syn.Blocks.Block_Type;
         Has_Element_Block : Syn.Blocks.Block_Type;
      begin
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Object ("Handle.Reference")));

         declare
            Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Reference",
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Handle",
                      Syn.Named_Subtype
                        (Table.Ada_Name & "_Handle")),
                 Result_Type =>
                   Db.Database_Package_Name
                 & "." & Table.Ada_Name & "_Reference",
                 Block       => Block);
         begin
            Target.Append (Fn);
         end;

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Reference_" & Table.Ada_Name,
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Handle",
                      Syn.Named_Subtype
                        (Table.Ada_Name & "_Handle")),
                 Result_Type =>
                   Db.Database_Package_Name
                 & "." & Table.Ada_Name & "_Reference",
                 Block       => Block);
         begin
            Fn.Set_Overriding;
            Target.Append (Fn);
         end;

         Has_Element_Block.Add_Declaration
           (Syn.Declarations.Use_Type
              (Db.Database_Package_Name & "."
               & Table.Ada_Name & "_Reference"));

         Has_Element_Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.Operator
                   ("/=", Syn.Object ("Handle.Reference"),
                    Syn.Object (Db.Database_Package_Name
                      & ".Null_" & Table.Ada_Name
                      & "_Reference"))));

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Has_Element",
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Handle",
                      Syn.Named_Subtype
                        (Table.Ada_Name & "_Handle")),
                 Result_Type => "Boolean",
                 Block       => Has_Element_Block);
         begin
            Fn.Set_Overriding;
            Target.Append (Fn);
         end;

      end Create_Reference_Functions;

      -----------------------------
      -- Create_Update_Functions --
      -----------------------------

      procedure Create_Update_Functions is
         Block : Syn.Blocks.Block_Type;
      begin
         Block.Append
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.New_Function_Call_Expression
                   ("Update_" & Table.Ada_Name,
                    Syn.Object ("Handle.Reference"))));

         declare
            Fn : constant Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Update",
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Handle",
                      Syn.Named_Subtype
                        (Table.Ada_Name & "_Handle")),
                 Result_Type =>
                   Table.Ada_Name & "_Update_Handle'Class",
            Block       => Block);
         begin
            Target.Append (Fn);
         end;

         declare
            Fn : Syn.Declarations.Subprogram_Declaration'Class :=
              Syn.Declarations.New_Function
                (Name        => "Update_" & Table.Ada_Name,
                 Argument    =>
                   Syn.Declarations.New_Formal_Argument
                     ("Handle",
                      Syn.Named_Subtype
                        (Table.Ada_Name & "_Handle")),
                 Result_Type =>
                   Table.Ada_Name
                 & "_Update_Handle'Class",
                 Block       => Block);
         begin
            Fn.Set_Overriding;
            Target.Append (Fn);
         end;

         declare
            procedure Create_Base_Update_Function
              (Base : Kit.Schema.Tables.Table_Type);

            ---------------------------------
            -- Create_Base_Update_Function --
            ---------------------------------

            procedure Create_Base_Update_Function
              (Base : Kit.Schema.Tables.Table_Type)
            is
               Block : Syn.Blocks.Block_Type;
            begin

               if Base.Has_Writable_Field then
                  Block.Append
                    (Syn.Statements.New_Procedure_Call_Statement
                       ("Cache.Invalidate",
                        Syn.Object ("Handle.Reference")));
                  Block.Append
                    (Syn.Statements.New_Return_Statement
                       (Syn.Expressions.New_Function_Call_Expression
                            (Base.Ada_Name & ".Update_" & Base.Ada_Name,
                             Syn.Object
                               ("Handle.Reference_"
                                & Base.Ada_Name))));

                  declare
                     use Syn.Declarations;
                     Fn : Subprogram_Declaration'Class :=
                       Syn.Declarations.New_Function
                         (Name        => "Update_" & Base.Ada_Name,
                          Argument    =>
                            Syn.Declarations.New_Formal_Argument
                              ("Handle",
                               Syn.Named_Subtype
                                 (Table.Ada_Name & "_Handle")),
                          Result_Type =>
                            Base.Ada_Name
                          & "." & Base.Ada_Name
                          & "_Update_Handle'Class",
                          Block       => Block);
                  begin
                     Fn.Set_Overriding;
                     Target.Append (Fn);
                  end;
               end if;
            end Create_Base_Update_Function;

         begin
            Table.Iterate (Create_Base_Update_Function'Access,
                           Inclusive => False);
         end;
      end Create_Update_Functions;

      ----------------------
      -- Return_Type_Name --
      ----------------------

      function Return_Type_Name
        (Field : Kit.Schema.Fields.Field_Type)
         return String
      is
         Return_Type : constant Kit.Schema.Types.Kit_Type :=
           Field.Get_Field_Type;
         Return_Name : constant String :=
           (if Return_Type.Is_Table_Reference
            then Db.Handle_Package_Name
            & "." & Return_Type.Ada_Name
            & "." & Return_Type.Ada_Name & "_Class"
            elsif Return_Type.Has_Custom_Type
            then Db.Database_Package_Name
            & "." & Return_Type.Ada_Name
            else Return_Type.Return_Handle_Subtype);
      begin
         return Return_Name;
      end Return_Type_Name;

   begin
      Table.Iterate
        (Process     => Add_Base'Access,
         Inclusive   => False);

      Table.Iterate_All (Add_Field_Type_With'Access,
                         Table_First => True);

      Interface_Definition.Add_Parent ("Handle_Interface");

      if False then
         Table.Iterate
           (Process     => Add_Base_Db'Access,
            Inclusive   => False);
      end if;

      if Table.Has_Writable_Field then
         Target.Append (Syn.Declarations.New_Separator);
         Kit.Generate.Updates.Create_Update_Type
           (Table  => Table,
            Target => Target);
      end if;

      Target.Append
        (Syn.Declarations.New_Full_Type_Declaration
           (Identifier => Interface_Name,
            Definition => Interface_Definition));

      Target.Append (Syn.Declarations.New_Separator);

      Target.Append
        (Syn.Declarations.New_Subtype_Declaration
           (Identifier => Class_Name,
            Definition => Syn.Named_Subtype (Interface_Name & "'Class")));

      Target.Append (Syn.Declarations.New_Separator);

      Create_Abstract_Reference_Functions;

      Table.Iterate (Create_Abstract_Property'Access);

      Target.Append (Syn.Declarations.New_Separator);

      Handle_Definition.Set_Visible_Derivation;
      Handle_Definition.Add_Parent (Interface_Name);

      Handle_Definition.Add_Component
        (Component_Name => "Reference",
         Component_Type =>
           Db.Database_Package_Name & "."
         & Table.Ada_Name & "_Reference",
         Component_Default =>
           Db.Database_Package_Name & "."
         & "Null_" & Table.Ada_Name & "_Reference");

      declare
         Handle_Type : constant Syn.Declarations.Type_Declaration :=
           Syn.Declarations.New_Private_Type_Declaration
             (Identifier => Handle_Name,
              Definition => Handle_Definition);
      begin
         Target.Append (Handle_Type);
      end;

      Target.Append (Syn.Declarations.New_Separator);
      Create_Get_Functions;

      Target.Append (Syn.Declarations.New_Separator);
      Create_Reference_Functions;

      Target.Append (Syn.Declarations.New_Separator);
      Table.Iterate (Process     => Create_Base_Conversion'Access,
                     Inclusive   => False);

      Create_From_Class_Conversion;

      if Table.Has_Writable_Field then
         Target.Append (Syn.Declarations.New_Separator);
         Kit.Generate.Updates.Generate_Update_Subprograms
           (Db     => Db,
            Table  => Table,
            Target => Target);
         Create_Update_Functions;
      end if;

      Target.Append (Syn.Declarations.New_Separator);
      Table.Iterate_All (Create_Property'Access);

      Create_Null_Handle_Function;

      if not Table.Is_Abstract then
         Create_New_Record_Function;
      end if;

   end Create_Handle_Type;

   --------------------------
   -- Create_Key_Functions --
   --------------------------

   procedure Create_Key_Functions
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Target : in out Syn.Declarations.Package_Type'Class)
   is
      procedure Create_Key_Get
        (Base  : Kit.Schema.Tables.Table_Type;
         Key   : Kit.Schema.Keys.Key_Type);

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
            Search.Create_Unique_Get_Function
              (Db            => Db,
               Table         => Table,
               Key_Table     => Base,
               Table_Package => Target,
               Key_Name      => Key.Standard_Name);
         else
            for Use_Key_Value in Boolean loop
               if Use_Key_Value then
                  if Key.Unique then
                     Search.Create_Unique_Get_Function
                       (Db            => Db,
                        Table         => Table,
                        Key_Table     => Base,
                        Table_Package => Target,
                        Key_Name      => Key.Standard_Name);
                  else
                     Search.Create_First_Last_Functions
                       (Db            => Db,
                        Table         => Table,
                        Key_Table     => Base,
                        Table_Package => Target,
                        Key_Name      => Key.Standard_Name);
                  end if;
               end if;

               Search.Create_Selection_Function
                 (Db            => Db,
                  Table         => Table,
                  Key_Table     => Base,
                  Table_Package => Target,
                  Key_Name      => Key.Standard_Name,
                  Key_Value     => Use_Key_Value,
                  Bounds        => False);
            end loop;

            for I in 1 .. Key.Field_Count loop
               if not Key.Field (I).Get_Field_Type.Is_Table_Reference then
                  Search.Create_Selection_Function
                    (Db            => Db,
                     Table         => Table,
                     Key_Table     => Base,
                     Table_Package => Target,
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
               Search.Create_Default_Key_Functions
                 (Db, Table, Target, Key);

            end if;
         end if;

      end Create_Key_Get;

   begin
      Search.Create_Selection_Type (Db, Table, Target);
      Table.Scan_Keys (Create_Key_Get'Access,
                       Include_Base_Keys => True);
   end Create_Key_Functions;

   ---------------------------------
   -- Find_Custom_Type_References --
   ---------------------------------

   function Find_Custom_Type_References
     (Table : Kit.Schema.Tables.Table_Type)
      return String_Vectors.Vector
   is
      Result : String_Vectors.Vector;

      procedure Check_Type
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      ----------------
      -- Check_Type --
      ----------------

      procedure Check_Type
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
      begin
         if Field.Get_Field_Type.Has_Custom_Type
           and then not Result.Contains (Field.Get_Field_Type.Ada_Name)
         then
            Result.Append (Field.Get_Field_Type.Ada_Name);
         end if;
      end Check_Type;

   begin
      Table.Iterate_All
        (Check_Type'Access);
      return Result;
   end Find_Custom_Type_References;

   --------------------------------------
   -- Find_Field_Type_Table_References --
   --------------------------------------

   function Find_Field_Type_Table_References
     (Table : Kit.Schema.Tables.Table_Type)
      return String_Vectors.Vector
   is
      Result : String_Vectors.Vector;

      procedure Check_Type
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type);

      ----------------
      -- Check_Type --
      ----------------

      procedure Check_Type
        (Base  : Kit.Schema.Tables.Table_Type;
         Field : Kit.Schema.Fields.Field_Type)
      is
         pragma Unreferenced (Base);
      begin
         if Field.Get_Field_Type.Is_Table_Reference
           and then Field.Get_Field_Type.Ada_Name /= Table.Ada_Name
           and then not Result.Contains (Field.Get_Field_Type.Ada_Name)
         then
            Result.Append (Field.Get_Field_Type.Ada_Name);
         end if;
      end Check_Type;

   begin
      Table.Iterate_All
        (Check_Type'Access);
      return Result;
   end Find_Field_Type_Table_References;

   -----------------------------
   -- Generate_Handle_Package --
   -----------------------------

   function Generate_Handle_Package
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : Syn.Declarations.Package_Type'Class)
      return Syn.Declarations.Package_Type'Class
   is
      Handle_Package : Syn.Declarations.Package_Type'Class :=
        Top.New_Child_Package
          (Table.Ada_Name);

      Referenced_Tables : constant String_Vectors.Vector :=
        Find_Field_Type_Table_References (Table);

      Referenced_Types  : constant String_Vectors.Vector :=
        Find_Custom_Type_References (Table);

   begin

      Handle_Package.With_Package
        (Db.Database_Package_Name);
      Handle_Package.With_Package
        ("Kit.Protected_Maps",
         Body_With => True);

      Handle_Package.With_Package
        (Db.Database_Package_Name
         & "." & Table.Ada_Name
         & "_Hashes",
         Body_With => True);

      if Table.Has_Text_Type then
         Handle_Package.With_Package
           ("Ada.Strings.Unbounded",
            Private_With => True);
      end if;

      if not Referenced_Tables.Is_Empty then
         Handle_Package.With_Package
           (Db.Database_Package_Name,
            Private_With => True);
      end if;

      for Name of Referenced_Tables loop
--           Handle_Package.With_Package
--             (Db.Handle_Package_Name & "." & Name);

         Handle_Package.Append
           (Syn.Declarations.New_Subtype_Declaration
              (Identifier => Name & "_Class",
               Definition =>
                 Syn.Named_Subtype
                   (Db.Handle_Package_Name
                    & "." & Name & "." & Name & "_Class")));

         declare
            Reference_Subtype : Syn.Declaration'Class :=
              Syn.Declarations.New_Subtype_Declaration
                (Identifier => Name & "_Reference",
                 Definition =>
                   Syn.Named_Subtype
                     (Db.Database_Package_Name
                      & "." & Name & "_Reference"));
         begin
            Reference_Subtype.Set_Private_Spec;
            Handle_Package.Append (Reference_Subtype);
         end;

      end loop;

      declare
         Reference_Subtype : Syn.Declaration'Class :=
           Syn.Declarations.New_Subtype_Declaration
             (Identifier => Table.Ada_Name & "_Reference",
              Definition =>
                Syn.Named_Subtype
                  (Db.Database_Package_Name
                   & "." & Table.Ada_Name & "_Reference"));
      begin
         Reference_Subtype.Set_Private_Spec;
         Handle_Package.Append (Reference_Subtype);
      end;

      for Name of Referenced_Types loop

         declare
            Reference_Subtype : Syn.Declaration'Class :=
              Syn.Declarations.New_Subtype_Declaration
                (Identifier => Name,
                 Definition =>
                   Syn.Named_Subtype
                     (Db.Database_Package_Name
                      & "." & Name));
         begin
            Reference_Subtype.Set_Private_Spec;
            Handle_Package.Append (Reference_Subtype);
         end;

      end loop;

      Handle_Package.Add_Separator;
      Create_Handle_Cache (Db, Table, Handle_Package);
      Create_Handle_Type (Db, Table, Handle_Package);

      Create_Key_Functions (Db, Table, Handle_Package);

      return Handle_Package;
   end Generate_Handle_Package;

end Kit.Generate.Handles;
