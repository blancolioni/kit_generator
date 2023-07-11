with Kit.Schema.Keys;
with Kit.Schema.Tables;

package body Kit.Generate.Marlowe_Keys_Package is

   ----------------------
   -- Generate_Package --
   ----------------------

   function Generate_Package
     (Db : Kit.Schema.Databases.Database_Type)
      return Syn.Declarations.Package_Type
   is
      Result : Syn.Declarations.Package_Type :=
                 Syn.Declarations. New_Package_Type
                   (Db.Ada_Name & ".Marlowe_Keys");

      procedure Generate_Table_Keys
        (Table : Kit.Schema.Tables.Table_Type);

      -------------------------
      -- Generate_Table_Keys --
      -------------------------

      procedure Generate_Table_Keys
        (Table : Kit.Schema.Tables.Table_Type)
      is
         procedure Generate_Key
           (Base : Kit.Schema.Tables.Table_Type;
            Key  : Kit.Schema.Keys.Key_Type);

         ------------------
         -- Generate_Key --
         ------------------

         procedure Generate_Key
           (Base : Kit.Schema.Tables.Table_Type;
            Key  : Kit.Schema.Keys.Key_Type)
         is
            pragma Unreferenced (Base);
            use Kit.Schema.Tables;
            Dec : constant Syn.Declaration'Class :=
                    Syn.Declarations.New_Object_Declaration
                      (Table.Key_Reference_Name (Key),
                       Syn.Named_Subtype
                         ("Marlowe.Data_Stores.Key_Reference"));
         begin
            Result.Append (Dec);
         end Generate_Key;

      begin
         Table.Scan_Keys (Generate_Key'Access);
      end Generate_Table_Keys;

   begin
      Result.With_Package ("Marlowe.Data_Stores");

      Result.Append
        (Syn.Declarations.New_Object_Declaration
           ("Handle",
            Syn.Named_Subtype
              ("Marlowe.Data_Stores.Data_Store")));

      Db.Iterate (Generate_Table_Keys'Access);

      return Result;
   end Generate_Package;

end Kit.Generate.Marlowe_Keys_Package;
