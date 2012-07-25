------------------------------------------------------------------------------
--
--     Deepend - Dynamic Storage Pools for Ada 95, Ada 2005 and Ada 2012
--
--            B A S I C   B O U N D E D   D Y N A M I C   P O O L S
--
--                                S p e c
--
--                  Copyright (C) 2011, Bradley J. Moore
--
--  Deepend is free software;  you can  redistribute it  and/or modify it
--  under  terms of the  GNU General Public License  as  published  by the
--  Free Software  Foundation;  either version 2,  or (at your option) any
--  later  version.  Paraffin is  distributed in the hope that it  will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU
--  General Public License for  more details.  You should have  received a
--  copy of the GNU General Public License distributed with Deepend;  see
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.
--
--  As a  special exception, if other files  instantiate generics from
--  this unit,  or you link this  unit with other files  to produce an
--  executable,  this unit  does  not by  itself  cause the  resulting
--  executable to be covered by  the GNU General Public License.  This
--  exception does  not however invalidate  any other reasons  why the
--  executable file might be covered by the GNU Public License.
------------------------------------------------------------------------------

--  Deepend provides storage pools with subpool capabilities for Ada95,
--  Ada 2005, and Ada 2012. Subpools provide an efficient mechanism to
--  release storage that is considered to be safer and more efficient than
--  other strategies such as garbage collection.
--
--  There are 4 Storage Pool packages to choose from in Deepend.
--
--     1) Dynamic_Pools
--     2) Bounded_Dynamic_Pools
--     3) Basic_Dynamic_Pools
--     4) Basic_Bounded_Dynamic_Pools
--
--  The Dynamic_Pools package has subpool capabilities where the storage
--  in each Subpool object is unbounded. If the current block of memory
--  is fully allocated to objects, and further objects are allocated,
--  then another block of storage is allocated to the subpool, and further
--  allocations to that subpool are carved out of that new storage block.
--
--  The Bounded_Dynamic_Pools package has subpool capabilities where the
--  storage in each Subpool object is bounded. If the Subpool is fully
--  allocated with objects, and an attempt is made to allocate further objects
--  from the same subpool, then a Storage_Error exception is raised.
--
--  The Basic_Dynamic_Pool package does not have subpool capabilities, and
--  each allocation is managed instead by the pool object.
--  A Basic_Dynamic_Pool is an unbounded pool such that if the current block
--  of storage is fully allocated with objects, and further objects are
--  allocated, then another block of memory is allocated to the pool, and
--  further object allocations are carved out of that new block,

--  The Basic_Bounded_Dynamic_Pool package does not have subpool capabilities,
--  and each allocation is managed instead by the pool object.
--  A Basic_Dynamic_Pool is a bounded pool such that if the pool's storage has
--  been fully allocated with objects, and an attemp is made to allocate
--  further objects, then a Storage_Error exception is raised.

--  In Ada 2012, the new allocation syntax may be used with this pool, to
--  specify the subpool that will contain the allocated objects.
--
--     e.g.
--          Object := new (subpool_name) Object_Type'(Value);
--
--  For Ada 95 and Ada 2005, the same effect can be obtained by using the
--  Allocation and Initialized_Allocation generics provided by this package.
--  However, these generics only allow allocating non-controlled objects of
--  definite types to a particular subpool, whereas in Ada 2012, indefinate
--  types and controlled types, and other types needing finalization such as
--  protected types may also be allocated to a subpool. Only task types or type
--  that have tasks cannot be allocated to a subpool.
--
--  In addition, for Ada 95, Ada 2005, and Ada 2012, the "new" keyword may be
--  used without specifying a subpool, which results in an object being
--  allocated to the default subpool for the storage pool.
--
--  A Dynamic_Pool allows objects allocated from a subpool to be reclaimed
--  all at once, instead of requiring each object to be individually
--  reclaimed one at a time via the Ada.Unchecked_Deallocation generic.
--  In fact, Ada.Unchecked_Deallocation is not needed or expected to be used
--  with this storage pool.
--
--  Tasks can create subpools from the same Dynamic Pool object at the
--  same time, but only one task may allocate objects from a specific subpool
--  instance at a time. A task "owns" its subpools, and attempts by other
--  tasks to allocate from subpool owned by another task results in
--  a Program_Error exception being raised.
--
--  There are separate versions of these packages for Ada95, Ada 2005, and
--  Ada 2012. The three versions were designed to have mostly compatible
--  interfaces, but there are slight differences in each newer language
--  version that takes advantages of newer language features.
--  In particular,
--    - for Ada 2005, the Scoped_Subpool type has a Create_Subpool constructor,
--      which allows the Block_Size to have a defaulted value.
--    - for Ada 2012, Pool parameters are in out parameters, rather than
--      access parameters, which eliminates the need to declare the pool
--      object as an aliased object.
--
--
--  Allocation strategy:
--
--    Deallocate is not needed or used, and is implemented as a null
--    procedure. Use of this storage pool means that there is no need for
--    calls to Ada.Unchecked_Deallocation.
--
--    The strategy is to provide an efficient storage pool that allocates
--    objects quickly with minimal overhead, and very fast dealloction.
--    Tasks "own" its subpool objects, which allows allocation from the
--    subpools to be more efficient, since there is no need for task
--    synchronization.
--
--    The intent is that the subpool strategy should generally outperform
--    other strategies such as garbage collection, or individual object
--    reclamation in a more deterministic fashion.
--
--  ** NOTE: In the Ada 2005 version of Dynamic_Pools, it is erroneous to
--    allocate objects that need finalization eg. (Tasks, protected types,
--    or objects of types inherited from types defined in Ada.Finalization)
--  and then deallocate the subpool associated with those objects before
--  they would have otherwise been finalized.

--  For Ada 2012, it is only erroneous to allocate task objects or objects
--  containing task components to a subpool.

with Ada.Task_Identification; use Ada.Task_Identification;
with System.Storage_Elements; use System;
with System.Storage_Pools;

package Basic_Bounded_Dynamic_Pools is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   Default_Size : constant := 16#FFFF#;
   --  A Block Size is the size of the heap allocation used when more
   --  storage is needed for the pool. Larger block sizes imply less
   --  heap allocations. Generally, better performance involves using larger
   --  block sizes.

   type Basic_Dynamic_Pool
     (Size : Storage_Elements.Storage_Count;
      Heap_Allocated : Boolean)
     is new Storage_Pools.Root_Storage_Pool with private;
   --  The Size specifies how much storage is managed by the pool.
   --  If Heap_Allocated is true, the storage is allocated from
   --  heap, otherwise the storage is directly in the Pool object.

   overriding function Storage_Size
     (Pool : Basic_Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the amount of storage managed by the pool.

   function Storage_Used
     (Pool : Basic_Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of storage allocated to
   --  objects from the pool.

   function Is_Owner
     (Pool : Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean;
   --  Returns True if the specified task "owns" the pool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Pool : in out Basic_Dynamic_Pool;
      T : Task_Id := Current_Task);
   --  Pre => (Is_Owner (Pool, Null_Task_Id) and then T = Current_Task)
   --         or else (Is_Owner (Pool) and then T = Null_Task_Id),
   --  Post => Is_Owner (Pool, T);
   --
   --  An Owning task can relinquish ownership of a pool by setting the
   --  owner to a Null_Task_Id. Another task may obtain ownership of a pool,
   --  provided that the pool has no owner.

private

   subtype Storage_Array is System.Storage_Elements.Storage_Array;

   type Storage_Array_Access is access Storage_Array;

   subtype Storage_Array_Index is System.Storage_Elements.Storage_Offset range
     1 .. System.Storage_Elements.Storage_Offset'Last;

   type Basic_Dynamic_Pool
     (Size : Storage_Elements.Storage_Count;
      Heap_Allocated : Boolean)
     is new Storage_Pools.Root_Storage_Pool with
      record
         Next_Allocation : Storage_Array_Index;
         Owner : Ada.Task_Identification.Task_Id;
         case Heap_Allocated is
            when True =>
               Active_Access : Storage_Array_Access;

            when False =>
               Active : Storage_Array (1 .. Size);
         end case;
      end record;

   use type Storage_Elements.Storage_Count;

   overriding
   procedure Allocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);
   --  with Pre => Is_Owner (Pool, Current_Task);

   overriding
   procedure Deallocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is null;

   overriding
   procedure Initialize (Pool : in out Basic_Dynamic_Pool);

   overriding
   procedure Finalize   (Pool : in out Basic_Dynamic_Pool);

   pragma Inline
     (Allocate,
      Initialize, Finalize, Is_Owner, Set_Owner);

end Basic_Bounded_Dynamic_Pools;
