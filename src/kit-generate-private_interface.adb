with System.Storage_Elements;

with WL.String_Sets;

with Syn.Blocks;
with Syn.Expressions;
with Syn.Statements;
with Syn.Types;

with Kit.Schema.Fields;
with Kit.Schema.Keys;
with Kit.Schema.Types;

package body Kit.Generate.Private_Interface is

   procedure Create_Compound_Key_To_Storage_Functions
     (Db     : Kit.Schema.Databases.Database_Type;
      Table  : Kit.Schema.Tables.Table_Type;
      Top    : in out Syn.Declarations.Package_Type'Class;
      Withed : in out WL.String_Sets.Set);

   procedure Create_Database_Record
     (Table : Kit.Schema.Tables.Table_Type;
      Impl  : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Memory_Record
     (Table : Kit.Schema.Tables.Table_Type;
      Impl  : in out Syn.Declarations.Package_Type'Class);
   pragma Unreferenced (Create_Memory_Record);

   procedure Create_Read_Write_Procedures
     (Table : Kit.Schema.Tables.Table_Type;
      Impl  : in out Syn.Declarations.Package_Type'Class);

   procedure Create_Key_Mutexes
     (Table : Kit.Schema.Tables.Table_Type;
      Impl  : in out Syn.Declarations.Package_Type'Class);

   ----------------------------------------------
   -- Create_Compound_Key_To_Storage_Functions --
   ----------------------------------------------

   procedure Create_Compound_Key_To_Storage_Functions
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top    : in out Syn.Declarations.Package_Type'Class;
      Withed : in out WL.String_Sets.Set)
   is
      pragma Unreferenced (Db);

      procedure Create_Key_To_Storage
        (Key   : Kit.Schema.Keys.Key_Type);

      procedure Create_Partial_Key_To_Storage
        (Key                 : Kit.Schema.Keys.Key_Type;
         Partial_Field_Count : Positive);

      function To_Storage_Expression
        (Key   : Kit.Schema.Keys.Key_Type;
         Index : Positive;
         Last  : Natural := 0)
         return Syn.Expression'Class;

      ---------------------------
      -- Create_Key_To_Storage --
      ---------------------------

      procedure Create_Key_To_Storage
        (Key   : Kit.Schema.Keys.Key_Type)
      is
         Block : Syn.Blocks.Block_Type;
      begin
         if Key.Field_Count = 1 then
            return;
         end if;

         Block.Add_Declaration
           (Syn.Declarations.Use_Type
              ("System.Storage_Elements.Storage_Array"));
         Block.Add_Statement
           (Syn.Statements.New_Return_Statement
              (To_Storage_Expression
                   (Key, 1)));

         declare
            use Syn, Syn.Declarations;
            Fn : Subprogram_Declaration'Class :=
                   Syn.Declarations.New_Function
                     (Key.Ada_Name & "_To_Storage",
                      "System.Storage_Elements.Storage_Array",
                      Block);
         begin
            for I in 1 .. Key.Field_Count loop
               declare
                  Key_Field_Type : constant Kit.Schema.Types.Kit_Type :=
                                     Key.Field (I).Get_Field_Type;
               begin
                  if Key_Field_Type.Is_External_Type then
                     declare
                        P : constant String :=
                              Key_Field_Type.External_Type_Package_Name;
                     begin
                        if not Withed.Contains (P) then
                           Top.With_Package (P);
                           Withed.Include (P);
                        end if;
                     end;
                  end if;

                  Fn.Add_Formal_Argument
                    (Key.Field (I).Ada_Name,
                     Key_Field_Type.Argument_Subtype);
               end;
            end loop;
            Top.Append (Fn);
         end;

         for I in 1 .. Key.Field_Count - 1 loop
            Create_Partial_Key_To_Storage (Key, I);
         end loop;

      end Create_Key_To_Storage;

      -----------------------------------
      -- Create_Partial_Key_To_Storage --
      -----------------------------------

      procedure Create_Partial_Key_To_Storage
        (Key                 : Kit.Schema.Keys.Key_Type;
         Partial_Field_Count : Positive)
      is
         Block          : Syn.Blocks.Block_Type;
         Missing_Length : Natural := 0;
      begin
         if Key.Field_Count = 1 then
            return;
         end if;

         Block.Add_Declaration
           (Syn.Declarations.Use_Type
              ("System.Storage_Elements.Storage_Array"));

         for I in Partial_Field_Count + 1 .. Key.Field_Count loop
            Missing_Length := Missing_Length
              + Key.Field (I).Size;
         end loop;
         Block.Add_Declaration
           (Syn.Declarations.New_Constant_Declaration
              ("Partial_Key_Suffix",
               "System.Storage_Elements.Storage_Array (1 .."
               & Missing_Length'Image & ")",
               Syn.Object
                 (Create_Aggregate
                      ("others => (if Fill_Low then 0 else 255)"))));

         Block.Add_Statement
           (Syn.Statements.New_Return_Statement
              (To_Storage_Expression
                   (Key, 1, Partial_Field_Count)));

         declare
            use Syn, Syn.Declarations;
            Fn : Subprogram_Declaration'Class :=
                   Syn.Declarations.New_Function
                     ("Partial_" & Key.Ada_Name & "_To_Storage",
                      "System.Storage_Elements.Storage_Array",
                      Block);
         begin
            Fn.Add_Formal_Argument
              ("Fill_Low", "Boolean");
            for I in 1 .. Partial_Field_Count loop
               declare
                  Key_Field_Type : constant Kit.Schema.Types.Kit_Type :=
                                     Key.Field (I).Get_Field_Type;
               begin
                  Fn.Add_Formal_Argument
                    (Key.Field (I).Ada_Name,
                     Key_Field_Type.Argument_Subtype);
               end;
            end loop;
            Top.Append (Fn);
         end;
      end Create_Partial_Key_To_Storage;

      ---------------------------
      -- To_Storage_Expression --
      ---------------------------

      function To_Storage_Expression
        (Key   : Kit.Schema.Keys.Key_Type;
         Index : Positive;
         Last  : Natural := 0)
         return Syn.Expression'Class
      is
         use Syn.Expressions;
         Field : constant Kit.Schema.Fields.Field_Type :=
                   Key.Field (Index);
         This  : constant Syn.Expression'Class :=
                   Field.Get_Field_Type.To_Storage_Array
                     (Field.Ada_Name);
      begin
         if Index = Last then
            return Operator
              ("&", This,
               Syn.Object ("Partial_Key_Suffix"));
         elsif Index = Key.Field_Count then
            return This;
         else
            return Operator ("&", This,
                             To_Storage_Expression (Key, Index + 1, Last));
         end if;
      end To_Storage_Expression;

   begin
      Table.Scan_Keys (Create_Key_To_Storage'Access);
   end Create_Compound_Key_To_Storage_Functions;

   ----------------------------
   -- Create_Database_Record --
   ----------------------------

   procedure Create_Database_Record
     (Table : Kit.Schema.Tables.Table_Type;
      Impl  : in out Syn.Declarations.Package_Type'Class)
   is

      Have_String_With : Boolean := False;
      Have_Text_With   : Boolean := False;

      Record_Defn : Syn.Types.Record_Type_Definition;

      procedure Add_Component (Field : Kit.Schema.Fields.Field_Type);
      procedure Add_Base_Index (Base : Kit.Schema.Tables.Table_Type);

      --------------------
      -- Add_Base_Index --
      --------------------

      procedure Add_Base_Index (Base : Kit.Schema.Tables.Table_Type) is
      begin
         Record_Defn.Add_Component ("T" & Base.Index_Image & "_Idx",
                                    "Marlowe.Database_Index");
      end Add_Base_Index;

      -------------------
      -- Add_Component --
      -------------------

      procedure Add_Component (Field : Kit.Schema.Fields.Field_Type) is
      begin
         if Field.Get_Field_Type.Has_Default_Value then
            Syn.Types.Add_Component
              (Record_Defn, Field.Ada_Name,
               Syn.Named_Subtype
                 (Field.Get_Field_Type.Record_Subtype),
               Field.Get_Field_Type.Default_Value);
         else
            Syn.Types.Add_Component
              (Record_Defn, Field.Ada_Name,
               Syn.Named_Subtype
                 (Field.Get_Field_Type.Record_Subtype));
         end if;

         if not Have_String_With
           and then Field.Get_Field_Type.Is_Bounded_String
         then
            Impl.With_Package ("Kit.Strings");
            Have_String_With := True;
         end if;

         if not Have_Text_With
           and then Field.Get_Field_Type.Is_Text
         then
            Impl.With_Package ("Kit.Text");
            Have_Text_With := True;
         end if;

      end Add_Component;

   begin

      Record_Defn.Add_Component
        ("Magic",
         Syn.Named_Subtype ("Integer"),
         Syn.Object (Table.Ada_Name & "_Magic"));

      Record_Defn.Add_Component
        ("Deleted",
         Syn.Named_Subtype ("Boolean"),
         Syn.Object ("False"));

      if False then
         Table.Iterate (Process     => Add_Base_Index'Access,
                        Inclusive   => False);
      end if;

      Table.Scan_Fields (Add_Component'Access);

      Impl.Append
        (Syn.Declarations.New_Full_Type_Declaration
           (Identifier => Table.Ada_Name & "_Database_Record",
            Definition => Record_Defn));

   end Create_Database_Record;

   ------------------------
   -- Create_Key_Mutexes --
   ------------------------

   procedure Create_Key_Mutexes
     (Table : Kit.Schema.Tables.Table_Type;
      Impl  : in out Syn.Declarations.Package_Type'Class)
   is

      procedure Add_Mutex (Base : Kit.Schema.Tables.Table_Type;
                           Key  : Kit.Schema.Keys.Key_Type);

      ---------------
      -- Add_Mutex --
      ---------------

      procedure Add_Mutex (Base : Kit.Schema.Tables.Table_Type;
                           Key  : Kit.Schema.Keys.Key_Type)
      is
         pragma Unreferenced (Base);
      begin
         Impl.Append
           (Syn.Declarations.New_Object_Declaration
              (Key.Ada_Name & "_Key_Mutex",
               "Kit.Mutex.Mutex_Type"));
      end Add_Mutex;

   begin
      Table.Scan_Keys (Add_Mutex'Access);

      Impl.Append
        (Syn.Declarations.New_Object_Declaration
           ("File_Mutex",
            "Kit.Mutex.Mutex_Type"));

   end Create_Key_Mutexes;

   --------------------------
   -- Create_Memory_Record --
   --------------------------

   procedure Create_Memory_Record
     (Table : Kit.Schema.Tables.Table_Type;
      Impl  : in out Syn.Declarations.Package_Type'Class)
   is

      Record_Defn : Syn.Types.Record_Type_Definition;

   begin

      Record_Defn.Add_Parent ("Kit.Cache.Cache_Entry_Record");
      Record_Defn.Add_Component
        ("Item",
         Table.Ada_Name & "_Database_Record",
         Is_Access => True);

      Impl.Append
        (Syn.Declarations.New_Full_Type_Declaration
           (Identifier => Table.Ada_Name & "_Memory_Record",
            Definition => Record_Defn));

      Impl.Add_Separator;
      Impl.Append
        (Syn.Declarations.New_Full_Type_Declaration
           (Identifier => "Cached_" & Table.Ada_Name,
            Definition =>
              Syn.New_Access_Type
                (Table.Ada_Name & "_Memory_Record'Class",
                 Access_All => True)));

   end Create_Memory_Record;

   ----------------------------------
   -- Create_Read_Write_Procedures --
   ----------------------------------

   procedure Create_Read_Write_Procedures
     (Table : Kit.Schema.Tables.Table_Type;
      Impl  : in out Syn.Declarations.Package_Type'Class)
   is

      function Call_Marlowe (Write : Boolean)
                             return Syn.Statement'Class;

      procedure Create_Transfer (Write : Boolean);

      ------------------
      -- Call_Marlowe --
      ------------------

      function Call_Marlowe (Write : Boolean)
                             return Syn.Statement'Class
      is
         use Syn;
         use Syn.Statements;
         Name : constant String := (if Write then "Write_Record"
                                    else "Get_Record");
         Stat : Procedure_Call_Statement :=
                  New_Procedure_Call_Statement
                    ("Marlowe_Keys.Handle." & Name);
      begin
         Stat.Add_Actual_Argument (Table.Ada_Name & "_Table_Index");
         Stat.Add_Actual_Argument ("Ref");
         Stat.Add_Actual_Argument ("Storage'Address");
         return Stat;
      end Call_Marlowe;

      ---------------------
      -- Create_Transfer --
      ---------------------

      procedure Create_Transfer (Write : Boolean) is

         use System.Storage_Elements;

         Block : Syn.Blocks.Block_Type;

         procedure Handle_Storage (Field : Kit.Schema.Fields.Field_Type);

         --------------------
         -- Handle_Storage --
         --------------------

         procedure Handle_Storage
           (Field : Kit.Schema.Fields.Field_Type)
         is
            Start  : constant Storage_Offset := Table.Field_Start (Field);
            Finish : constant Storage_Offset :=
                       Table.Field_Start (Field) +
                       Storage_Offset (Field.Size) - 1;
            Rec    : constant String :=
                       "Item." & Field.Ada_Name;
         begin
            Block.Append
              (Field.Get_Field_Type.Storage_Array_Transfer
                 (Object_Name       => Rec,
                  To_Storage        => Write,
                  Storage_Name      => "Storage",
                  Start             => Start,
                  Finish            => Finish));
         end Handle_Storage;

      begin
         Block.Add_Declaration
           (Syn.Declarations.New_Object_Declaration
              ("Storage",
               "System.Storage_Elements.Storage_Array "
               & "(0 .."
               & System.Storage_Elements.Storage_Offset'Image
                 (Table.Length - 1)
               & ")"));

         if Write then
            Table.Scan_Fields (Handle_Storage'Access);
            --  Table.Iterate (Handle_Base'Access, Inclusive => False);
         end if;

         Block.Add_Statement (Call_Marlowe (Write));

         if not Write then
            Table.Scan_Fields (Handle_Storage'Access);
            --  Table.Iterate (Handle_Base'Access, Inclusive => False);
         end if;

         declare
            P     : Syn.Declarations.Subprogram_Declaration'Class :=
                      Syn.Declarations.New_Procedure
                        (Name  => (if Write then "Write" else "Read"),
                         Block => Block);
            Mode  : constant Syn.Declarations.Argument_Mode :=
                      (if Write then Syn.Declarations.In_Argument
                       else Syn.Declarations.Out_Argument);
         begin
            P.Add_Formal_Argument ("Ref", "Marlowe.Database_Index");
            P.Add_Formal_Argument ("Item", Mode,
                                   Table.Implementation_Record_Type);

            Impl.Append (P);
         end;
      end Create_Transfer;

   begin

      for Write in Boolean loop
         Create_Transfer (Write);
      end loop;

   end Create_Read_Write_Procedures;

   --------------------------------
   -- Generate_Private_Interface --
   --------------------------------

   function Generate_Private_Interface
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : Syn.Declarations.Package_Type'Class)
      return Syn.Declarations.Package_Type'Class
   is
      Impl_Package : Syn.Declarations.Package_Type'Class :=
                       Top.New_Child_Package (Table.Ada_Name & "_Impl");
   begin
      Impl_Package.Set_Private;
      --  Impl_Package.With_Package ("Kit.Cache");
      Impl_Package.With_Package ("Kit.Mutex");
      Impl_Package.With_Package ("System.Storage_Elements");
      Impl_Package.With_Package ("Marlowe.Key_Storage",
                                 Body_With => True);
      Impl_Package.With_Package (Db.Ada_Name & ".Marlowe_Keys",
                                Body_With => True);
      Impl_Package.Append
        (Syn.Declarations.New_Constant_Declaration
           (Table.Ada_Name & "_Magic",
            Syn.Literal (Table.Magic_Number)));
      Impl_Package.Add_Separator;

      Create_Database_Record (Table, Impl_Package);
      Impl_Package.Add_Separator;

--        if Table.Has_Local_Key_Field then
--           Impl_Package.Append_To_Body
--             (Syn.Declarations.Use_Type
--                ("System.Storage_Elements.Storage_Array"));
--        end if;
--
      declare
         Block : Syn.Blocks.Block_Type;
      begin
         Block.Add_Declaration
           (Syn.Declarations.Use_Type
              ("System.Storage_Elements.Storage_Offset"));
         Block.Add_Statement
           (Syn.Statements.New_Return_Statement
              (Syn.Expressions.Operator
                 ("/",
                  Syn.Object
                    (Table.Ada_Name & "_Database_Record'Size"),
                  Syn.Object ("System.Storage_Unit"))));

         Impl_Package.Append
           (Syn.Declarations.New_Function
              ("Disk_Storage_Units",
               "System.Storage_Elements.Storage_Count",
               Block));
      end;
--        Create_Memory_Record (Table, Impl_Package);
--        Impl_Package.Add_Separator;

      Create_Read_Write_Procedures (Table, Impl_Package);
      Create_Key_Mutexes (Table, Impl_Package);

      declare
         External_Withs : WL.String_Sets.Set;
      begin
         Create_Compound_Key_To_Storage_Functions
           (Db, Table, Impl_Package, External_Withs);
      end;

      return Impl_Package;
   end Generate_Private_Interface;

end Kit.Generate.Private_Interface;
