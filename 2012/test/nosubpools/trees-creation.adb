--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore

package body Trees.Creation is

   function Create
     (Item : Integer;
      Depth : Integer) return Tree_Node_Access
   is
      Result : constant Tree_Node_Access := new Tree_Node;
   begin
      if Depth > 0 then
         Result.all :=
           (Left => Create (2 * Item - 1, Depth - 1).all'Unchecked_Access,
            Right => Create (2 * Item, Depth - 1).all'Unchecked_Access,
            Value => Item);
      else
         Result.all := (Left => null, Right => null, Value => Item);
      end if;

      return Result;

   end Create;

end Trees.Creation;
