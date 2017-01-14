--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore

package body Bounded_Trees_Ada2012 is

   package Node_Allocators is new
     Subpool_Allocators
       (Allocation_Type        => Node,
        Allocation_Type_Access => Tree_Node);

   function Create
     (Subpool : Subpool_Handle;
      Item : Integer;
      Depth : Integer) return Tree_Node is

      function Recurse
        (Item : Integer;
         Depth : Integer) return Tree_Node
      is
         function Allocate_Node return Tree_Node
         is
            --  A constant Boolean will conditionally compile in the selected
            --  code
            Use_Ada2012_Subpool_Allocator_Syntax : constant Boolean := True;
         begin
            pragma Warnings (Off, "*code can never be executed*");

            if Use_Ada2012_Subpool_Allocator_Syntax then
               return new (Subpool) Node;
            else
               return Node_Allocators.Allocate (Subpool);
            end if;

            pragma Warnings (On, "*code can never be executed*");

            pragma Compile_Time_Warning
              (True,
               "Ada 2012 subpool allocator syntax is currently" &
                 " slower in GNAT than the Ada 2005 approach");
         end Allocate_Node;

         Result : constant Tree_Node := Allocate_Node;
      begin

         if Depth > 0 then
            Result.all := (Left => Recurse (2 * Item - 1, Depth - 1),
                           Right => Recurse (2 * Item, Depth - 1),
                           Value => Item);
         else
            Result.all := (Left | Right => null, Value => Item);
         end if;

         return Result;

      end Recurse;

   begin
      return Recurse (Item, Depth);
   end Create;

   function Item_Check (Item : Tree_Node) return Integer is
   begin
      if Item.Left = null then
         return Item.Value;
      else
         return Item.Value + Item_Check (Item.Left) - Item_Check (Item.Right);
      end if;
   end Item_Check;
end Bounded_Trees_Ada2012;
