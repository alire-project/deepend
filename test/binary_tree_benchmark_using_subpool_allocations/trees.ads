--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore
with Dynamic_Pools.Subpools; use Dynamic_Pools;

package Trees is

   type Tree_Node is private;
   function Item_Check (Item : Tree_Node) return Integer;

   function Create
     (Pool : Subpools.Dynamic_Pool_With_Subpools;
      Item : Integer;
      Depth : Integer) return Tree_Node;

private

   type Node;
   type Tree_Node is access all Node;

   type Node is record
      Left  : Tree_Node;
      Right : Tree_Node;
      Value  : Integer;
   end record;

end Trees;
