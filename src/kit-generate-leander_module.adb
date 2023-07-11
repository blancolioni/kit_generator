with Ada.Text_IO;

with Kit.Install;

with Kit.Schema.Fields;
with Kit.Schema.Keys;
with Kit.Schema.Tables;
with Kit.Schema.Types;

package body Kit.Generate.Leander_Module is

   procedure Create_Record_Type (Db   : Kit.Schema.Databases.Database_Type;
                                 File : Ada.Text_IO.File_Type);
   pragma Unreferenced (Create_Record_Type);

   procedure Create_Table
     (Table : Kit.Schema.Tables.Table_Type;
      File  : Ada.Text_IO.File_Type);

   ------------------------
   -- Create_Record_Type --
   ------------------------

   procedure Create_Record_Type (Db   : Kit.Schema.Databases.Database_Type;
                                 File : Ada.Text_IO.File_Type)
   is

      First : Boolean := True;

      procedure Add_Item (Item : Kit.Schema.Tables.Table_Type);

      --------------
      -- Add_Item --
      --------------

      procedure Add_Item (Item : Kit.Schema.Tables.Table_Type) is
      begin
         if First then
            Ada.Text_IO.Put (File, "    ");
            First := False;
         else
            Ada.Text_IO.Put (File, "  | ");
         end if;
         Ada.Text_IO.Put_Line (File, "R_" & Item.Ada_Name);
      end Add_Item;

   begin
      Ada.Text_IO.Put_Line (File, "data RecordType =");
      Db.Iterate (Add_Item'Access);
   end Create_Record_Type;

   ------------------
   -- Create_Table --
   ------------------

   procedure Create_Table
     (Table : Kit.Schema.Tables.Table_Type;
      File  : Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;

      Have_Base : Boolean := False;

      procedure Put_Base_Class (Base : Kit.Schema.Tables.Table_Type);

      procedure Put_Field (Field : Kit.Schema.Fields.Field_Type);

      procedure Put_Key (Key : Kit.Schema.Keys.Key_Type);

      procedure Put_Key_Get
        (Base : Kit.Schema.Tables.Table_Type;
         Key  : Kit.Schema.Keys.Key_Type);

      procedure Put_Key_Select
        (Base : Kit.Schema.Tables.Table_Type;
         Key  : Kit.Schema.Keys.Key_Type);

      procedure Put_Class_Instance
        (Base : Kit.Schema.Tables.Table_Type);

      --------------------
      -- Put_Base_Class --
      --------------------

      procedure Put_Base_Class (Base : Kit.Schema.Tables.Table_Type) is
      begin
         if Have_Base then
            Put (File, ", ");
         else
            Put (File, "(");
         end if;
         Put (File, Base.Haskell_Name & "Class a");
         Have_Base := True;
      end Put_Base_Class;

      ------------------------
      -- Put_Class_Instance --
      ------------------------

      procedure Put_Class_Instance
        (Base : Kit.Schema.Tables.Table_Type)
      is

         procedure Put_Field_Fetch
           (Field  : Kit.Schema.Fields.Field_Type);

         ---------------------
         -- Put_Field_Fetch --
         ---------------------

         procedure Put_Field_Fetch
           (Field  : Kit.Schema.Fields.Field_Type)
         is
            Type_Name : constant String :=
                          Field.Get_Field_Type.Haskell_Type_Name;
            Is_Reference : constant Boolean :=
                             Field.Get_Field_Type.Is_Table_Reference;
         begin
            Put (File,
                 "  " & Base.Haskell_Variable_Name & Field.Haskell_Name);
            Put (File, " (" & Table.Haskell_Name & " r) = ");

            if Is_Reference or else Type_Name = "Int" then
               Put (File, "getIntField");
            elsif Type_Name = "Float" then
               Put (File, "getFloatField");
            else
               Put (File, "getField");
            end if;
            Put (File,
                 " " & Table.Index_Image & " r """
                 & Field.Standard_Name & """");
            if Is_Reference then
               Put (File,
                    " >>= (\x -> return ("
                    & Field.Get_Field_Type.Haskell_Name
                    & " x))");
            end if;
            New_Line (File);
         end Put_Field_Fetch;

      begin
         New_Line (File);
         Put_Line (File,
                   "instance " & Base.Haskell_Name & "Class "
                   & Table.Haskell_Name & " where");
         Base.Iterate (Put_Field_Fetch'Access);
      end Put_Class_Instance;

      ---------------
      -- Put_Field --
      ---------------

      procedure Put_Field (Field : Kit.Schema.Fields.Field_Type) is
      begin
         Put_Line
           (File,
            "  " & Table.Haskell_Variable_Name & Field.Haskell_Name
            & " :: a -> IO "
            & Field.Get_Field_Type.Haskell_Type_Name);
      end Put_Field;

      -------------
      -- Put_Key --
      -------------

      procedure Put_Key (Key : Kit.Schema.Keys.Key_Type) is
      begin
         if Key.Field_Count = 1 then
            Put_Line
              (File,
               "  " & Table.Haskell_Variable_Name
               & "GetBy" & Key.Haskell_Name
               & " :: "
               & Key.Field (1).Get_Field_Type.Haskell_Type_Name
               & " -> IO a");
         end if;
      end Put_Key;

      -----------------
      -- Put_Key_Get --
      -----------------

      procedure Put_Key_Get
        (Base : Kit.Schema.Tables.Table_Type;
         Key  : Kit.Schema.Keys.Key_Type)
      is
         pragma Unreferenced (Base);
         Type_Name    : constant String :=
                          Key.Field (1).Get_Field_Type.Haskell_Type_Name;
         Is_Reference : constant Boolean :=
                          Key.Field (1).Get_Field_Type.Is_Table_Reference;
      begin
         if Key.Field_Count = 1 then
            Put (File,
                 Table.Haskell_Variable_Name
                 & "GetBy"
                 & Key.Haskell_Name
                 & " ");
            if Is_Reference then
               Put (File, "(" & Type_Name & " k)");
            else
               Put (File, "k");
            end if;
            Put (File, " = ");
            Put (File,
                 "getBy " & Table.Index_Image & " """
                 & Key.Standard_Name & """ ");

            if Type_Name /= "String" then
               Put (File, "(show k)");
            else
               Put (File, "k");
            end if;

            Put (File,
                 " >>= (return . "
                 & Table.Haskell_Name
                 & ")");

            New_Line (File);

         end if;
      end Put_Key_Get;

      --------------------
      -- Put_Key_Select --
      --------------------

      procedure Put_Key_Select
        (Base : Kit.Schema.Tables.Table_Type;
         Key  : Kit.Schema.Keys.Key_Type)
      is
         pragma Unreferenced (Base);
         Type_Name    : constant String :=
                          Key.Field (1).Get_Field_Type.Haskell_Type_Name;
         Is_Reference : constant Boolean :=
                          Key.Field (1).Get_Field_Type.Is_Table_Reference;
      begin
         if Key.Field_Count = 1 then
            Put (File,
                 Table.Haskell_Variable_Name
                 & "SelectBy"
                 & Key.Haskell_Name
                 & " ");
            if Is_Reference then
               Put (File, "(" & Type_Name & " k)");
            else
               Put (File, "k");
            end if;
            Put (File, " = ");
            Put (File,
                 "selectBy " & Table.Index_Image & " """
                 & Key.Standard_Name & """ ");

            if Type_Name /= "String" then
               Put (File, "(show k)");
            else
               Put (File, "k");
            end if;

            Put (File,
                 " >>= (\x -> return (map "
                 & Table.Haskell_Name
                 & " x))");

            New_Line (File);

         end if;
      end Put_Key_Select;

   begin
      New_Line (File);
      Put (File, "class ");
      Table.Iterate (Put_Base_Class'Access, Inclusive => False);
      if Have_Base then
         Put (File, ") => ");
      end if;
      Put_Line (File, Table.Haskell_Name & "Class a where");

      Table.Scan_Fields (Put_Field'Access);

      if False then
         Table.Scan_Keys (Put_Key'Access);
      end if;

      New_Line (File);
      Put_Line (File,
                "data " & Table.Haskell_Name & " = "
                & Table.Haskell_Name & " Int");

      Table.Iterate (Put_Class_Instance'Access, Inclusive => True);

      Table.Scan_Keys (Put_Key_Get'Access);
      Table.Scan_Keys (Put_Key_Select'Access);

   end Create_Table;

   -----------------------------
   -- Generate_Leander_Module --
   -----------------------------

   procedure Generate_Leander_Module
     (Db    : Kit.Schema.Databases.Database_Type;
      Path  : String)
   is
      use Ada.Text_IO;
      Kit_Module_File     : File_Type;
      Leander_Module_File : File_Type;
   begin
      Create (Leander_Module_File, Out_File,
              Path & "/" & Db.Ada_Name & ".hs");

      Open (Kit_Module_File,
            In_File,
            Kit.Install.Library_Path & "/Kit.hs");
      while not End_Of_File (Kit_Module_File) loop
         Put_Line (Leander_Module_File, Get_Line (Kit_Module_File));
      end loop;
      Close (Kit_Module_File);

      New_Line (Leander_Module_File);

      declare
         procedure Call_Create_Table
           (Table : Kit.Schema.Tables.Table_Type);

         -----------------------
         -- Call_Create_Table --
         -----------------------

         procedure Call_Create_Table
           (Table : Kit.Schema.Tables.Table_Type)
         is
         begin
            Create_Table (Table, Leander_Module_File);
         end Call_Create_Table;
      begin
         Db.Iterate (Call_Create_Table'Access);
      end;

      Close (Leander_Module_File);

   end Generate_Leander_Module;

end Kit.Generate.Leander_Module;
