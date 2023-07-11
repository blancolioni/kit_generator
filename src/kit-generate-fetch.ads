with Syn;

with Kit.Schema.Tables;

package Kit.Generate.Fetch is

   procedure Fetch_From_Index
     (Table       : Kit.Schema.Tables.Table_Type;
      Object_Name : String;
      Update      : Boolean;
      Target      : in out Syn.Statement_Sequencer'Class);

end Kit.Generate.Fetch;
