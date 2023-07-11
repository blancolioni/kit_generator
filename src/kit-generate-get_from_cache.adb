with Syn;

package body Kit.Generate.Get_From_Cache is

   -----------------------------
   -- Generate_Get_From_Cache --
   -----------------------------

   function Generate_Get_From_Cache
     (Db    : Kit.Schema.Databases.Database_Type;
      Table : Kit.Schema.Tables.Table_Type;
      Top   : Syn.Declarations.Package_Type'Class)
      return Syn.Declarations.Package_Type'Class
   is
      Database_Package      : constant String :=
                                 Table.Ada_Name & "_Impl";
      Table_Package : Syn.Declarations.Package_Type'Class :=
        Top.New_Child_Package (Table.Ada_Name & "_Cache");
   begin
      Table_Package.Set_Private;
      Table_Package.With_Package ("Kit.Generic_Cache");
      Table_Package.With_Package
        (Db.Ada_Name & "." & Database_Package);
      Table_Package.Set_Generic_Instantiation ("Kit.Generic_Cache");

      Table_Package.Add_Generic_Actual_Argument
        (Integer (Table.Reference_Index));
      Table_Package.Add_Generic_Actual_Argument
        (Table.Magic_Number);

      Table_Package.Add_Generic_Actual_Argument
        (Database_Package & "." & Table.Ada_Name
         & "_Database_Record");
      Table_Package.Add_Generic_Actual_Argument
        (Database_Package & ".Read");
      Table_Package.Add_Generic_Actual_Argument
        (Database_Package & ".Write");

      return Table_Package;
   end Generate_Get_From_Cache;

end Kit.Generate.Get_From_Cache;
