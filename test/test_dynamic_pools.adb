with Dynamic_Pools.Subpools; use Dynamic_Pools;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Dynamic_Pools
is
   Pool : aliased Subpools.Dynamic_Pool_With_Subpools
     (Mode => Subpools.Auto_Unchecked_Deallocation,
      Declaring_Task_Is_Owner => True);

   type Node_Type is record
      Value : Integer;
      Next : access Node_Type;
   end record;

   type Node_Access is access Node_Type;
   for Node_Access'Storage_Pool use Pool;

   function New_Node is new Dynamic_Pools.Allocation
     (Node_Type,
      Node_Access);

   pragma Warnings (off, "*Sub_Pool*is not referenced");

   function Recurse (Pool : access Subpools.Dynamic_Pool_With_Subpools;
                     Depth : Natural) return Node_Access
   is
      Sub_Pool : aliased Subpools.Dynamic_Pool_With_Subpools
        := Subpools.Create_Subpool (Pool);
      Node : constant Node_Access := New_Node (Sub_Pool'Access);
   begin
      if Depth = 0 then
         Node.all := (0, null);
         return  Node;
      else
         Node.all := (Depth, Recurse (Sub_Pool'Access, Depth - 1));
         return Node;
      end if;
   end Recurse;

   pragma Warnings (on, "*Sub_Pool*is not referenced");

   procedure Print (List : Node_Type)
   is
   begin
      if List.Next /= null then
         Print (List.Next.all);
      end if;
      Put_Line (Integer'Image (List.Value));
   end Print;

   List : constant Node_Access := Recurse (Pool'Access, 10);
begin
   Print (List.all);
end Test_Dynamic_Pools;
