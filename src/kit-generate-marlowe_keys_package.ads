with Syn.Declarations;

package Kit.Generate.Marlowe_Keys_Package is

   function Generate_Package
     (Db : Kit.Schema.Databases.Database_Type)
      return Syn.Declarations.Package_Type;

end Kit.Generate.Marlowe_Keys_Package;
