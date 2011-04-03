--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore
package Trees is

   type Tree_Node is private;
   function Item_Check (Item : access Tree_Node) return Integer;

private

   type Tree_Node is record
      Left  : access Tree_Node;
      Right : access Tree_Node;
      Value  : Integer;
   end record;

end Trees;
