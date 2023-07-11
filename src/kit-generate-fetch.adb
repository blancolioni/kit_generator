with Syn.Expressions;
with Syn.Statements;

package body Kit.Generate.Fetch is

   ----------------------
   -- Fetch_From_Index --
   ----------------------

   procedure Fetch_From_Index
     (Table       : Kit.Schema.Tables.Table_Type;
      Object_Name : String;
      Update      : Boolean;
      Target      : in out Syn.Statement_Sequencer'Class)
   is

      procedure Lock_Base (Base   : Kit.Schema.Tables.Table_Type);
      procedure Get_Base (Base   : Kit.Schema.Tables.Table_Type);
      procedure Unlock_Base (Base   : Kit.Schema.Tables.Table_Type);

      --------------
      -- Get_Base --
      --------------

      procedure Get_Base (Base   : Kit.Schema.Tables.Table_Type) is
         use Syn;
         use Syn.Expressions;
         use Syn.Statements;

         Base_Target    : constant String :=
                            Object_Name & Base.Base_Component_Name;
         Cache_Package  : constant String :=
                            Base.Ada_Name & "_Cache";
         Index_Variable : constant String :=
                            Table.Database_Index_Component
                              (Object_Name, Base);
      begin
         Lock_Base (Base);
         Target.Append
           (New_Assignment_Statement
              (Base_Target,
               Operator
                 (".",
                  New_Function_Call_Expression
                    (Cache_Package & ".Get",
                     New_Function_Call_Expression
                       ("Marlowe.Database_Index",
                        Index_Variable),
                     Object ("False")),
                  Object ("Db"))));
      end Get_Base;

      ---------------
      -- Lock_Base --
      ---------------

      procedure Lock_Base (Base   : Kit.Schema.Tables.Table_Type) is
         use Syn.Statements;
         Index_Variable : constant String :=
                            Table.Database_Index_Component
                              (Object_Name, Base);
         Lock_Procedure : constant String :=
                            (if Update
                             then Base.Ada_Name & "_Cache.U_Lock"
                             else Base.Ada_Name & "_Cache.S_Lock");
      begin
         Target.Append
           (New_Procedure_Call_Statement
              (Lock_Procedure,
               Syn.Expressions.New_Function_Call_Expression
                 ("Marlowe.Database_Index", Index_Variable)));
      end Lock_Base;

      -----------------
      -- Unlock_Base --
      -----------------

      procedure Unlock_Base (Base   : Kit.Schema.Tables.Table_Type) is
         use Syn.Statements;
         Index_Variable : constant String :=
                            Table.Database_Index_Component
                              (Object_Name, Base);
      begin
         Target.Append
           (New_Procedure_Call_Statement
              (Base.Ada_Name & "_Cache.Unlock",
               Syn.Expressions.New_Function_Call_Expression
                 ("Marlowe.Database_Index", Index_Variable)));
      end Unlock_Base;

   begin
      if Option_Debug then
         declare
            use Syn;
            use Syn.Expressions;
            use Syn.Statements;
         begin
            Target.Append
              (New_Procedure_Call_Statement
                 ("Ada.Text_IO.Put_Line",
                  Operator
                    ("&",
                     Literal
                       ("Fetch " & Table.Ada_Name & ": "),
                     New_Function_Call_Expression
                       ("To_String",
                        Object (Object_Name & ".M_Index")))));
         end;
      end if;

      Table.Iterate (Get_Base'Access,
                     Inclusive   => True,
                     Table_First => True);
      if not Update then
         Table.Iterate (Unlock_Base'Access,
                        Inclusive   => True,
                        Table_First => True);
      end if;
   end Fetch_From_Index;

end Kit.Generate.Fetch;
