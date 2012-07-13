--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore

generic
   type Tree_Node_Access is access Tree_Node;
package Trees.Creation is

   function Create
     (Item : Integer;
      Depth : Integer) return Tree_Node_Access;

end Trees.Creation;
