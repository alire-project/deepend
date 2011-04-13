--  The Computer Language Benchmarks Game
--  http://shootout.alioth.debian.org/
--
--  Based on Ada versions created by
--    Jim Rogers and Brian Drummond as well as the
--    C version by Francesco Abbate
--
--  Contributed by Brad Moore
--
--  Note: This version of the code uses Deepends ability to allocate from
--  different pool objects using the same access type. In other words,
--  allocations are performed by calling the deepends "allocate" generic
--  rather than use the "new" operator.

--  The requirements of the benchmark are;
--
--    * define a tree node class and methods, a tree node record and
--      procedures, or an algegraic data type and functions
--    * allocate a binary tree to 'stretch' memory, check it exists,
--      and deallocate it
--    * allocate a long-lived binary tree which will live-on while
--      other trees are allocated and deallocated
--    * allocate, walk, and deallocate many bottom-up binary trees
--          - allocate a tree
--          - walk the tree nodes, checksum the node items (and maybe
--            deallocate the node)
--          - deallocate the tree
--    * check that the long-lived binary tree still exists
--
--  Note: this is an adaptation of a benchmark for testing GC so we are
--     interested in the whole tree being allocated before any nodes are GC'd
--     - which probably excludes lazy evaluation.
--
--  Note: the left subtrees are heads of the right subtrees, keeping a depth
--     counter in the accessors to avoid duplication is cheating!
--
--  Note: the tree should have tree-nodes all the way down, replacing the
--  bottom nodes by some other value is not acceptable; and the bottom nodes
--  should be at depth 0.
--
--  Note: these programs are being measured with the default initial heap size
--     - the measurements may be very different with a larger initial heap
--       size or GC tuning.
--
--  Please don't implement your own custom memory pool or free list.
--  **** Note: The intent here is to not use a special customized memory pool
--  to solve this problem, but using another general purpose memory pool is OK.
--  This version of the code uses Deepend, which is a memory pool wrapper
--  for the Apache Runtime Pool. The version of code actually submitted for
--  benchmark does not use Deepend, but instead uses low-level direct bindings
--  to the APR pools library.
--
--  The binary-trees benchmark is a simplistic adaptation of Hans Boehm's
--  GCBench, which in turn was adapted from a benchmark by John Ellis and
--  Pete Kovac.

with Trees;                  use Trees;
with Dynamic_Pools;          use Dynamic_Pools;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

procedure Binary_Trees is

   Pool : aliased Dynamic_Pools.Dynamic_Pool
     (Minimum_Allocation => 16#3FF#);

   function Get_Depth return Positive is
   begin
      if Argument_Count > 0 then
         return Positive'Value (Argument (1));
      else
         return 10;
      end if;
   end Get_Depth;

   function Get_Worker_Count return Positive is
   begin
      if Argument_Count > 1 then
         return Positive'Value (Argument (2));
      else
         --  This seems to be the sweet spot assuming max depth of 20
         return 5;
      end if;
   end Get_Worker_Count;

   Min_Depth     : constant := 4;
   Requested_Depth : constant Positive := Get_Depth;
   Max_Depth     : constant Positive := Positive'Max (Min_Depth + 2,
                                                      Requested_Depth);
   Depth_Iterations : constant Positive := (Max_Depth - Min_Depth) / 2 + 1;

   Worker_Count     : constant Positive := Get_Worker_Count;

   task type Depth_Worker
     (Start, Finish : Positive := Positive'Last) is
      pragma Storage_Size (16#100#);
   end Depth_Worker;

   Results : array (1 .. Depth_Iterations) of Integer;
   Iteration_Tracking : array (1 .. Depth_Iterations) of Positive;

   task body Depth_Worker
   is
      Depth         : Natural;
      Check         : Integer;
      Iterations    : Positive;
   begin

      for Depth_Iter in Start .. Finish loop

         Depth := Min_Depth + (Depth_Iter - 1) * 2;
         Iterations := 2 ** (Max_Depth - Depth + Min_Depth);
         Iteration_Tracking (Depth_Iter) := Iterations;

         Check      := 0;

         for I in 1 .. Iterations loop
            declare
               Short_Lived_Subpool : constant Subpool_Handle
                 := Create_Subpool (Pool'Access);
               Bomb : Scope_Bomb (Short_Lived_Subpool);
               pragma Unreferenced (Bomb);
               Short_Lived_Tree_1, Short_Lived_Tree_2 : Tree_Node;
            begin

               Short_Lived_Tree_1 :=
                 Create
                   (Short_Lived_Subpool,
                    Item  => I,
                    Depth => Depth);

               Short_Lived_Tree_2 :=
                  Create
                    (Short_Lived_Subpool,
                     Item  => -I,
                     Depth => Depth);

               Check := Check +
                 Item_Check (Short_Lived_Tree_1) +
                 Item_Check (Short_Lived_Tree_2);

            end;
         end loop;

         Results (Depth_Iter) := Check;
      end loop;

   end Depth_Worker;

   subtype Worker_Id is Positive range 1 .. Worker_Count;

   Start_Index         : Positive := 1;
   End_Index           : Positive := Depth_Iterations;

   Iterations_Per_Task : constant Positive :=
     Depth_Iterations / Worker_Count;

   Remainder           : Natural :=
     Depth_Iterations rem Worker_Count;

   function Create_Worker return Depth_Worker is
   begin
      if Remainder = 0 then
         End_Index := Start_Index + Iterations_Per_Task - 1;
      else
         End_Index := Start_Index + Iterations_Per_Task;
         Remainder := Remainder - 1;
      end if;

      return New_Worker : Depth_Worker
        (Start => Start_Index,
         Finish => End_Index)
      do
         Start_Index := End_Index + 1;
      end return;
   end Create_Worker;

   Long_Lived_Tree      : Tree_Node;

   Check : Integer;

begin

   --  Do the stretch tree processing at the same time that the long lived
   --  tree is being created.
   declare
      task Stretch_Depth_Task is
      end Stretch_Depth_Task;

      task body Stretch_Depth_Task is
         Stretch_Depth : constant Positive := Max_Depth + 1;

         Subpool : constant Subpool_Handle := Create_Subpool (Pool'Access);
         Bomb : Scope_Bomb (Subpool);
         pragma Unreferenced (Bomb);

         Stretch_Tree : constant Tree_Node :=
           Trees.Create (Subpool  => Subpool,
                         Item  => 0,
                         Depth => Stretch_Depth);
      begin
         Check        := Item_Check (Stretch_Tree);
         Put ("stretch tree of depth ");
         Put (Item => Stretch_Depth, Width => 1);
         Put (HT & " check: ");
         Put (Item => Check, Width => 1);
         New_Line;
      end Stretch_Depth_Task;

      task Create_Long_Lived_Tree_Task is
      end Create_Long_Lived_Tree_Task;

      task body Create_Long_Lived_Tree_Task is
         Subpool : constant Subpool_Handle := Pool.Create_Subpool;
      begin
         Long_Lived_Tree := Create (Subpool, 0, Max_Depth);
      end Create_Long_Lived_Tree_Task;
   begin
      null;
   end;

   --  Now process the trees of different sizes in parallel and collect results
   declare
      Workers : array (Worker_Id) of Depth_Worker
        := (others => Create_Worker);
      pragma Unreferenced (Workers);
   begin
      null;
   end;

   --  Now output the results
   for I in Results'Range loop
      Put (Item => Iteration_Tracking (I) * 2, Width => 0);
      Put (HT & " trees of depth ");
      Put (Item => Min_Depth + 2 * (I - 1), Width => 0);
      Put (HT & " check: ");
      Put (Item => Results (I), Width => 0);
      New_Line;
   end loop;

   --  Verify that the long lived tree still exists
   Put ("long lived tree of depth ");
   Put (Item => Max_Depth, Width => 0);
   Put (HT & " check: ");
   Check := Item_Check (Long_Lived_Tree);
   Put (Item => Check, Width => 0);
   New_Line;

end Binary_Trees;
