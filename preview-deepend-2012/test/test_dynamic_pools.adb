with Dynamic_Pools.Subpools; use Dynamic_Pools;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Dynamic_Pools
is
   Pool : aliased Subpools.Dynamic_Pool_With_Subpools
     (Mode => Subpools.Auto_Unchecked_Deallocation,
      Declaring_Task_Is_Owner => True);

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

   function New_Node is new Subpools.Allocation
     (Node_Type,
      Node_Access);

   function New_String is new Subpools.Initialized_Allocation
     (Allocation_Type => Id_String,
      Allocation_Type_Access => Id_String_Access);

   function Recurse (Pool : access Subpools.Dynamic_Pool_With_Subpools;
                     Depth : Natural) return Node_Access
   is
      Sub_Pool : aliased Subpools.Dynamic_Pool_With_Subpools
        := Subpools.Create_Subpool (Pool);
      Node : constant Node_Access := New_Node (Sub_Pool'Access);
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
      for String_Access'Storage_Pool use Sub_Pool;
      Name : constant String_Access :=
        new String'("Depth=" & Natural'Image (Depth));

      Description : constant Id_String_Access
        := New_String (Pool => Sub_Pool'Access,
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
                      Next => Recurse (Sub_Pool'Unchecked_Access, Depth - 1));
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

   List : constant Node_Access := Recurse (Pool'Unchecked_Access, 10);
begin
   Print (List.all);
end Test_Dynamic_Pools;
