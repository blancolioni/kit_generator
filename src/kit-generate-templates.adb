with Ada.Directories;
with Ada.Strings.Fixed;

with Kit.Install;
with Kit.Schema.Tables;
with Kit.Templates;

package body Kit.Generate.Templates is

   function Template_Path
     (File_Name : String)
      return String
   is (Ada.Directories.Compose
       (Ada.Directories.Compose
          (Kit.Install.Library_Path, "templates"),
          File_Name));

   ----------------------------
   -- Copy_Template_Packages --
   ----------------------------

   procedure Copy_Template_Packages
     (Database         : Kit.Schema.Databases.Database_Type;
      Target_Directory : String)
   is

      procedure General_Packages;

      procedure Extra_Packages
        (Table : Kit.Schema.Tables.Table_Type);

      --------------------
      -- Extra_Packages --
      --------------------

      procedure Extra_Packages
        (Table : Kit.Schema.Tables.Table_Type)
      is
         use Kit.Templates;

         Standard_Sub : Substitutions;

         procedure Copy (Base_Name : String);

         ----------
         -- Copy --
         ----------

         procedure Copy (Base_Name : String) is
            Package_Name : constant String :=
              Database.Ada_Name
              & "."
              & Table.Ada_Name
              & "_"
              & Base_Name;
            Source_Spec  : constant String := Base_Name & ".ads";
            Source_Body  : constant String := Base_Name & ".adb";
            Target_Spec  : constant String :=
              Make_File_Name (Package_Name, "ads");
            Target_Body  : constant String :=
              Make_File_Name (Package_Name, "adb");
            Source_Spec_Path : constant String := Template_Path (Source_Spec);
            Source_Body_Path : constant String := Template_Path (Source_Body);
         begin
            Copy_File (Source_Spec_Path,
                       Ada.Directories.Compose
                         (Target_Directory, Target_Spec),
                       Standard_Sub);
            if Ada.Directories.Exists (Source_Body_Path) then
               Copy_File (Source_Body_Path,
                          Ada.Directories.Compose
                            (Target_Directory, Target_Body),
                          Standard_Sub);
            end if;
         end Copy;

      begin

         Add_Substitution (Standard_Sub, "database", Database.Ada_Name);
         Add_Substitution (Standard_Sub, "handles",
                           Database.Handle_Package_Name);
         Add_Substitution (Standard_Sub, "table", Table.Ada_Name);
         Add_Substitution
           (Standard_Sub, "table_count",
            Ada.Strings.Fixed.Trim
              (Natural'Image (Database.Table_Count), Ada.Strings.Left));

         Copy ("hashes");

         if Table.With_Vector_Package then
            Copy ("vectors");
         end if;
         if Table.With_Map_Package then
            Copy ("maps");
         end if;
      end Extra_Packages;

      ----------------------
      -- General_Packages --
      ----------------------

      procedure General_Packages is
         use Kit.Templates;

         Standard_Sub : Substitutions;

         procedure Copy (Base_Name : String);

         ----------
         -- Copy --
         ----------

         procedure Copy (Base_Name : String) is
            Package_Name : constant String :=
                             Database.Ada_Name
                             & "."
                             & Base_Name;
            Source_Spec  : constant String :=
                             Make_File_Name (Base_Name, "ads");
            Source_Body  : constant String :=
                             Make_File_Name (Base_Name, "adb");
            Target_Spec  : constant String :=
                             Make_File_Name (Package_Name, "ads");
            Target_Body  : constant String :=
                             Make_File_Name (Package_Name, "adb");
         begin
            Copy_File (Template_Path (Source_Spec),
                       Target_Directory & "/" & Target_Spec,
                       Standard_Sub);
            Copy_File (Template_Path (Source_Body),
                       Target_Directory & "/" & Target_Body,
                       Standard_Sub);
         end Copy;

      begin

         Add_Substitution (Standard_Sub, "database", Database.Ada_Name);
         Add_Substitution (Standard_Sub, "handles",
                           Database.Handle_Package_Name);
         Add_Substitution
           (Standard_Sub, "table_count",
            Ada.Strings.Fixed.Trim
              (Natural'Image (Database.Table_Count), Ada.Strings.Left));

         Copy ("Kit_Deferred_Keys");
         Copy ("Kit_Locking");
         Copy ("Tables");
         Copy ("Tables.Scanner");

      end General_Packages;

   begin
      General_Packages;
      Database.Iterate
        (Extra_Packages'Access);
   end Copy_Template_Packages;

end Kit.Generate.Templates;
