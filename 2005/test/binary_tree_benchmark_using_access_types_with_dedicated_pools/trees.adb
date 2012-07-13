--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore

package body Trees is

   function Item_Check (Item : access Tree_Node) return Integer is
   begin
      if Item.Left = null then
         return Item.Value;
      else
         return Item.Value + Item_Check (Item.Left) - Item_Check (Item.Right);
      end if;
   end Item_Check;

end Trees;
