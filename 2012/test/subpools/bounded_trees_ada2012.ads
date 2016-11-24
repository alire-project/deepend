--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore
with Bounded_Dynamic_Pools; use Bounded_Dynamic_Pools;
with System.Storage_Elements; use System;

pragma Elaborate_All (Bounded_Dynamic_Pools);

package Bounded_Trees_Ada2012 is

   type Tree_Node is private;

   function Item_Check (Item : Tree_Node) return Integer;

   function Create
     (Subpool : Subpool_Handle;
      Item : Integer;
      Depth : Integer) return Tree_Node;

   Node_Size : constant Storage_Elements.Storage_Count;

   Pool : Bounded_Dynamic_Pools.Dynamic_Pool
     (Default_Subpool_Size => 0,
      Maximum_Subpools => 100);

private

   type Node;
   type Tree_Node is access all Node;
   for Tree_Node'Storage_Pool use Pool;
   pragma No_Strict_Aliasing (Tree_Node);

   type Node is record
      Left  : Tree_Node;
      Right : Tree_Node;
      Value  : Integer;
   end record;

   Node_Size : constant Storage_Elements.Storage_Count :=
     Node'Max_Size_In_Storage_Elements;

end Bounded_Trees_Ada2012;
