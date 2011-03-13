with Pool_Mark_Release; use Pool_Mark_Release;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Mark_Release
is
   Pool : aliased Unbounded_Mark_Release_Pool
     (Mode => Auto_Unchecked_Deallocation,
      Declaring_Task_Allocates => True);
   type Node_Type is record
      Value : Integer;
      Next : access Node_Type;
   end record;

   type Node_Access is access Node_Type;
   for Node_Access'Storage_Pool use Pool;

   function New_Node is new Pool_Mark_Release.Allocation
     (Node_Type,
      Node_Access);

   function Recurse (Pool : access Unbounded_Mark_Release_Pool;
                     Depth : Natural) return Node_Access
   is
      Sub_Pool : aliased Unbounded_Mark_Release_Pool :=
        Create_Subpool (Pool);
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
end Test_Mark_Release;
