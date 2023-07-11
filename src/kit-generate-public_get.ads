with Syn.Declarations;
with Kit.Schema.Keys;
with Kit.Schema.Tables;

package Kit.Generate.Public_Get is

   procedure Create_Selection_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String;
      Key_Value     : Boolean;
      Bounds        : Boolean;
      Bounded_Index : Natural   := 0);

   --  Generate a function which gets a selection from the given Table.
   --  Key_Table should be the table which originally declared the key.
   --  If Key_Value is True, the function returns a selection
   --  containing only records with a given value.  If Bounds is False,
   --  the selection contains the entire table in key order.
   --  If Bounds is True, a function is generated which accepts values
   --  first 1 .. Bounded_Index - 1 key components, followed by
   --  a lower and an upper bound for the Bounded_Index component.
   --  A selection matching these constraints, with the remaining
   --  components open, is returned.

   procedure Create_Unique_Get_Function
     (Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String);

   --  For table T, unique key K of type (t1,...,tn), generate a function
   --     function Get_By_K (v1 : t2; ...; vn : tn) return T
   --  Returns the record containing K if it exists

   procedure Create_First_Last_Functions
     (Table         : Kit.Schema.Tables.Table_Type;
      Key_Table     : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Name      : String);

   --  For table T, key K of type (t1,...,tn), generate a function
   --     function First_By_K (v1 : t2; ...; vn : tn) return T
   --  Returns the record containing K if it exists

   procedure Create_Iterator
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   --  Create the implementation of the iterator interface for selections.

   procedure Create_Reference_Get_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class);

   --  Generate a function which returns a record corresponding to
   --  a particular table reference (which is internally converted
   --  to a database index).

   procedure Create_Get_From_Index
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class)
     with Unreferenced;

   --  Generate a procedure which fetches a record given its index

   procedure Create_Default_Key_Functions
     (Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key           : Kit.Schema.Keys.Key_Type);

   procedure Create_Generic_Get_Function
     (Db            : Kit.Schema.Databases.Database_Type;
      Table         : Kit.Schema.Tables.Table_Type;
      Table_Package : in out Syn.Declarations.Package_Type'Class;
      Key_Value     : Boolean);

end Kit.Generate.Public_Get;
