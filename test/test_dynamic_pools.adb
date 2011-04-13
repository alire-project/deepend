with Dynamic_Pools; use Dynamic_Pools;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Dynamic_Pools
is
   Pool : aliased Dynamic_Pools.Dynamic_Pool
     (Minimum_Allocation => 4_096);

   subtype Id_String is String (1 .. 10);
   type Id_String_Access is access Id_String;

   type Node_Type is record
      Value : Integer;
      Name : access String;
      Description : Id_String_Access;
      Next : access Node_Type;
   end record;

   type Node_Access is access Node_Type;
   for Node_Access'Storage_Pool use Pool;

   function New_Node is new Dynamic_Pools.Allocation
     (Node_Type,
      Node_Access);

   function New_String is new Dynamic_Pools.Initialized_Allocation
     (Allocation_Type => Id_String,
      Allocation_Type_Access => Id_String_Access);

   function Recurse (Depth : Natural) return Node_Access
   is
      Sub_Pool : constant Dynamic_Pools.Subpool_Handle
        := Dynamic_Pools.Create_Subpool (Pool'Access);
      Node : constant Node_Access := New_Node (Sub_Pool);
      --  Note: Objects of String_Access type are finalized before returning
      --  However, String_Access type does not need finalization, and
      --  Unchecked_Deallocation on the String_Access doesn't do anything,
      --  (See Dynamic_Pool spec).
      --  We are obtaining Unchecked_Access of the string below, and because
      --  the storage pool lifetime of a subpool extends to that of its parent
      --  the anonymous string access should be guaranteed to point to the
      --  memory for the lifetime of the top level storage pool.
      --  ie. Is is safe to print out the tree after all recursion has
      --  completed.
      type String_Access is access String;
      for String_Access'Storage_Pool use Pool;
      Name : constant String_Access :=
        new String'("Depth=" & Natural'Image (Depth));

      Description : constant Id_String_Access
        := New_String (Subpool => Sub_Pool,
                       Qualified_Expression => "ABCDEFGHIJ");

   begin
      if Depth = 0 then
         Node.all := (Value => 0,
                      Name => Name.all'Unchecked_Access,
                      Description => Description,
                      Next => null);
         return  Node;
      else
         Node.all := (Value => Depth,
                      Name => Name.all'Unchecked_Access,
                      Description => Description,
                      Next => Recurse (Depth - 1));
         return Node;
      end if;
   end Recurse;

   procedure Print (List : Node_Type)
   is
   begin
      if List.Next /= null then
         Print (List.Next.all);
      end if;
      Put_Line (Integer'Image (List.Value) &
                ", Name=<" & List.Name.all &
                ">, Desc=<" & List.Description.all & '>');
   end Print;

   List : constant Node_Access := Recurse (10);
begin
   Print (List.all);
end Test_Dynamic_Pools;
