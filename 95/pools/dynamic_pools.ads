------------------------------------------------------------------------------
--
--     Deepend - Dynamic Storage Pools for Ada 95, Ada 2005 and Ada 2012
--
--                        D Y N A M I C   P O O L S
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

--  Deepend is a suite of dynamic storage pools with subpool capabilities for
--  Ada 95, Ada 2005, and Ada 2012. The Deepend storage pools were designed to
--  provide efficient concurrency for use in both single core and multicore
--  environments. The Deepend pools also provide flexibility such that
--  the storage for the pools may be placed entirely in static memory,
--  on the stack, or on the heap, or in various combinations of the above
--  (See the Bounded Pools below for more details on how this can be applied).
--
--  Further, Deepend ensures that each pool and subpool is "owned" by a
--  single Ada task, and thus only that task may allocate objects from a
--  particular subpool, and only one task may allocate objects directly to the
--  pool. Ownership of a pool and subpools may be relinquished and transferred
--  to other tasks. This capability eliminates the need for expensive locking
--  when allocating and deallocating objects. Attempts by other tasks to
--  allocate from a pool or subpool owned by another task results in a
--  Program_Error exception being raised.
--
--  Storage pools with subpool capabilities allow all objects in a subpool
--  to be reclaimed all at once, instead of requiring each object to be
--  individually reclaimed one at a time. A Dynamic Pool may have any number of
--  subpools. If subpools are not reclaimed prior to finalization of the pool,
--  then they are reclaimed when the pool is finalized.
--
--  With this Storage pool, Unchecked_Deallocation is implemented as a No-Op
--  (null procedure), because it is not needed or intended to be used.
--  If early finalization is needed, Unchecked_Deallocate_Subpool may be
--  used, which has similar issues as Unchecked_Deallocation, but is
--  safer, since it can be applied more globally, and less frequently. Even
--  Unchecked_Deallocate_Subpool is unnecessary for reclaiming subpools in
--  nested scopes with Deepend, as a scoped subpool facility is also provided,
--  which automatically finalizes subpools, when leaving the scope of their
--  declaration.
--
--  Subpool based storage management provides a safer means of memory
--  management, which can outperform other mechanisms for storage
--  reclamation including garbage collection.
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
--  is fully allocated to objects and further objects are allocated,
--  then another block of storage is allocated to the subpool, and further
--  allocations to that subpool are carved out of that new storage block.
--
--  The Bounded_Dynamic_Pools package has subpool capabilities where the
--  storage in each Subpool object is bounded, and the number of subpools
--  that may be allocated is also bounded. If the Subpool is fully
--  allocated with objects and an attempt is made to allocate further objects
--  from the same subpool, then a Storage_Error exception is raised. Similarly,
--  if an attempt is made to allocate more subpools than the maximum number
--  the pool was configured to manage, then a Storage_Error exception is
--  raised. A Bounded_Dynamic_Pools pool does not utilize the heap for its
--  management of subpools. Scoped_Subpool objects are provided that may be
--  configured to allocate their storage from the heap, or declared on the
--  stack, or statically at library level. In particular, Scoped_Subpool
--  objects are included that have discriminants that provide this control.
--  A scoped subpool automatically is finalized when execution leaves the scope
--  of the declaration. For a scoped subpool declared at library level, the
--  the storage remains available while the partition is active.
--
--  The Basic_Dynamic_Pool package does not have subpool capabilities, and
--  each allocation is managed instead by the pool object. When the pool is
--  finalized, all objects allocated from the pool that need finalization are
--  also finalized. A Basic_Dynamic_Pool is an unbounded pool such that if the
--  current block of storage is fully allocated with objects and further
--  objects are allocated, then another block of memory is allocated to the
--  pool, and further object allocations are carved out of that new block.
--
--  The Basic_Bounded_Dynamic_Pool package does not have subpool capabilities,
--  and each allocation is managed instead by the pool object. Like the
--  Basic_Dynamic_Pool, when the pool is finalized, all objects allocated from
--  the pool are also finalized. A Basic_Dynamic_Pool is a bounded pool such
--  that if the pool's storage has been fully allocated with objects and an
--  attempt is made to allocate further objects, then a Storage_Error exception
--  is raised. A Basic_Bounded_Dynamic_Pool pool has discriminants that
--  indicate whether the storage for the pool resides on the heap or
--  on the stack, or statically at library level.
--
--  Both Basic_Dynamic_Pools and Bounded_Basic_Dynamic_Pools are completely
--  forward compatible with the Ada 2012 standard for Storage_Pools, since they
--  only allow allocations via the existing "new" operator (without subpool
--  specifications). This facility relies on access type finalization to
--  finalize all the objects from a pool, and does not otherwise support
--  subpools.
--
--  In Ada 2012, the new allocation syntax may be used with this pool, to
--  specify the subpool that will contain the allocated objects.
--
--     e.g.
--          Object := new (subpool_name) Object_Type'(Value);
--
--  For Ada 95 and Ada 2005, a similar effect can be obtained by using the
--  Allocation and Initialized_Allocation generics provided by this package.
--  However, these generics only allow allocating non-controlled objects of
--  definite types to a particular subpool, whereas in Ada 2012, indefinite
--  types and controlled types, and other types needing finalization such as
--  protected types may also be allocated to a subpool. Only task types or
--  types that have tasks cannot be allocated to a subpool.
--
--  In addition, for Ada 95, Ada 2005, and Ada 2012, the "new" keyword may be
--  used without specifying a subpool, which results in an object being
--  allocated to the default subpool for the storage pool.
--
--  A Dynamic_Pool allows objects allocated from a subpool to be reclaimed
--  all at once, instead of requiring each object to be individually
--  reclaimed one at a time via the Ada.Unchecked_Deallocation generic.
--  In fact, Ada.Unchecked_Deallocation is not needed or expected to be used
--  with this storage pool (and has no effect).
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
--    calls to Ada.Unchecked_Deallocation (as it has no effect).
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
--  ** NOTE: In the Ada 95 and Ada 2005 version of Dynamic_Pools, it is
--    erroneous to allocate objects to a subpool that need finalization eg.
--    (Tasks, protected types, or objects of types inherited from types defined
--    in Ada.Finalization) and then deallocate the subpool associated with
--    those objects before they would have otherwise been finalized.
--
--  For Ada 2012, it is only erroneous to allocate task objects or objects
--  containing task components to a subpool.
--

with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Finalization;

with System.Storage_Elements; use System;
with Sys.Storage_Pools.Subpools;
use Sys;

package Dynamic_Pools is

   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   subtype Subpool_Handle is Storage_Pools.Subpools.Subpool_Handle;

   Default_Allocation_Block_Size : constant := 16#FFFF#;
   --  A Block Size is the size of the heap allocation used when more
   --  storage is needed for a subpool. Larger block sizes imply less
   --  heap allocations. Generally, better performance involves using larger
   --  block sizes.

   type Dynamic_Pool (Default_Block_Size : Storage_Elements.Storage_Count) is
     new Storage_Pools.Subpools.Root_Storage_Pool_With_Subpools
   with private;
   --  The Default_Block_Size is the Block Size associated with the default
   --  subpool, and with the overriding Create_Subpool call. A value of zero
   --  implies that a default subpool is not needed and therefore is not
   --  created. However, in that case, the overriding Create_Subpool call will
   --  use the Default_Allocation_Block_Size value for the Block Size
   --  If a different block size is needed for a subpool, there is another
   --  Create_Subpool variant that allows the block size to be specified.

   function Create_Subpool
     (Pool : access Dynamic_Pool) return Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.
   --  Uses the Default_Block_Size of the Pool when more storage is needed,
   --  except if Pool.Default_Block_Size is zero, then the
   --  Default_Allocation_Block_Size value is used.

   function Create_Subpool
     (Pool : access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count)
      return Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.

   function Storage_Size
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of storage allocated from the pool
   --  and its subpools, including storage that is allocated but not used.

   function Storage_Size
     (Subpool : Subpool_Handle) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of storage allocated from the
   --  subpool, including storage that is allocated but not used.

   function Storage_Used
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of storage allocated to objects from the
   --  pool and its subpools. It assumes all currently filled blocks are fully
   --  allocated, but returns the exact amount for the current active block
   --  for each subpool.

   function Storage_Used
     (Subpool : Subpool_Handle) return Storage_Elements.Storage_Count;
   --  Indicates the current approximate amount of storage allocated to
   --  objects from the subpool. It assumes all currently filled blocks are
   --  fully allocated, but returns the exact amount for the current active
   --  block.

   function Is_Owner
     (Pool : Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean;
   --  Returns True if the specified task "owns" the pool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Pool : in out Dynamic_Pool;
      T : Task_Id := Current_Task);
   --  Pre => (Is_Owner (Pool, Null_Task_Id) and then T = Current_Task)
   --         or else (Is_Owner (Pool) and then T = Null_Task_Id),
   --  Post => Is_Owner (Pool, T);
   --
   --  An Owning task can relinquish ownership of a pool by setting the
   --  owner to a Null_Task_Id. Another task may obtain ownership of a pool,
   --  provided that the pool has no owner.

   function Is_Owner
     (Subpool : Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean;
   --  Returns True if the specified task "owns" the pool/subpool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Subpool : Subpool_Handle;
      T : Task_Id := Current_Task);
   --  with Pre => (Is_Owner (Subpool, Null_Task_Id) and then T = Current_Task)
   --              or else (Is_Owner (Subpool) and then T = Null_Task_Id),
   --       Post => Is_Owner (Subpool, T);
   --  An Owning task can relinquish ownership of a subpool by setting the
   --  owner to a Null_Task_Id. Another task may obtain ownership of a subpool,
   --  provided that the subpool has no owner.

   procedure Unchecked_Deallocate_Subpool
     (Subpool : in out Subpool_Handle);
   --  This call performs unchecked storage deallocation of all objects
   --  allocated from the subpool, then destroys the subpool, setting
   --  Subpool to null.

   function Default_Subpool_For_Pool
     (Pool : access Dynamic_Pool) return Subpool_Handle;
   --  This calls returns the default subpool for the pool. It raises
   --  Storage_Error if Pool.Default_Block_Size is zero. The default
   --  subpool is used when Ada's "new" operator is used without specifying
   --  a subpool handle.

   function Has_Default_Subpool
     (Pool : Dynamic_Pool) return Boolean;
   --  Returns True if Pool currently has a default subpool, False otherwise

   use type Storage_Elements.Storage_Count;

   procedure Create_Default_Subpool
     (Pool : in out Dynamic_Pool);
   --  May be used to reinstate a default subpool if the default subpool has
   --  been deallocated.
   --  The task calling Create_Default_Subpool initially "owns" the subpool.
   --  Uses the Default_Block_Size of the Pool when more storage is needed,
   --  except if Pool.Default_Block_Size is zero, then the
   --  Default_Allocation_Block_Size value is used.

   type Scoped_Subpool
     (Pool : access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count) is tagged limited private;
   --  Scoped subpools define a controlled object that wraps a subpool
   --  handle, that automatically deallocates the subpool when the
   --  Scoped_Subpool_Handle object is finalized. Typically, the
   --  Create_Subpool call returning this type will be used to place an
   --  object in a nested scope.

   function Handle
     (Subpool : Scoped_Subpool) return Subpool_Handle;

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access all Allocation_Type;
   package Subpool_Allocators is

      function Default_Value return Allocation_Type;

      function Allocate
        (Subpool : Subpool_Handle;
         Value   : Allocation_Type := Default_Value)
         return Allocation_Type_Access;
      --  This generic routine provides a mechanism to allocate an object of
      --  a definite subtype from a specific subpool, and initializing the
      --  new object with a specific value.

      function Allocate
        (Subpool : Scoped_Subpool;
         Value   : Allocation_Type := Default_Value)
         return Allocation_Type_Access;
      --  This generic routine provides a mechanism to allocate an object of
      --  a definite subtype from a specific scoped subpool, and initializing
      --  the new object with a specific value.

   private
      Default : Allocation_Type;
   end Subpool_Allocators;

private

   use Ada;

   type Dynamic_Subpool;
   type Dynamic_Subpool_Access is access all Dynamic_Subpool;

   type Subpool_Array is array (Positive range <>) of Dynamic_Subpool_Access;
   type Subpool_Array_Access is access Subpool_Array;

   type Subpool_Vector is record
      Subpool_List : Subpool_Array_Access;
      Last : Natural := 0;
   end record;

   protected type Subpool_Set is

      procedure Initialize;
      procedure Add (Subpool : Dynamic_Subpool_Access);
      procedure Delete (Subpool : Dynamic_Subpool_Access);
      procedure Deallocate_All;
      function Storage_Usage return Storage_Elements.Storage_Count;
      function Storage_Total return Storage_Elements.Storage_Count;

   private
      Subpools : Subpool_Vector;
      pragma Inline (Add);
   end Subpool_Set;

   type Storage_Array_Access is access System.Storage_Elements.Storage_Array;

   subtype Storage_Array_Index is System.Storage_Elements.Storage_Offset range
     0 .. System.Storage_Elements.Storage_Offset'Last;

   type Vector is array (Storage_Array_Index range <>) of Storage_Array_Access;
   type Vector_Access is access Vector;

   type Storage_Vector is record
      Storage : Vector_Access;
      Last : Storage_Array_Index;
   end record;

   type Dynamic_Subpool
     (Block_Size : Storage_Elements.Storage_Count) is
     new Storage_Pools.Subpools.Root_Subpool with
      record
         Used_List : Storage_Vector;
         Free_List : Storage_Vector;
         Active : Storage_Array_Access;
         Next_Allocation : Storage_Array_Index;
         Owner : Ada.Task_Identification.Task_Id;
      end record;

   type Scoped_Subpool
     (Pool : access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count) is
     new Finalization.Limited_Controlled with
      record
         Handle : Subpool_Handle;
      end record;

   procedure Initialize (Subpool : in out Scoped_Subpool);
   procedure Finalize (Subpool : in out Scoped_Subpool);

   type Dynamic_Pool (Default_Block_Size : Storage_Elements.Storage_Count)
     is new Storage_Pools.Subpools.Root_Storage_Pool_With_Subpools
   with record
      Default_Subpool : Subpool_Handle;
      Subpools : Subpool_Set;
      Owner : Ada.Task_Identification.Task_Id;
   end record;

   procedure Allocate
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : Subpool_Handle);
   --  with Pre => Is_Owner (Subpool, Current_Task);

   procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool;
      Subpool : in out Subpool_Handle);
   --  Deallocate the space for all of the objects allocated from the
   --  specified subpool, and destroy the subpool. The subpool handle
   --  is set to null after this call.

   procedure Initialize (Pool : in out Dynamic_Pool);
   --  Create the default subpool if Pool.Default_Block_Size is non-zero

   procedure Finalize   (Pool : in out Dynamic_Pool);

   --  Note: In standard Ada, one should be able to list all of these
   --  subprograms in a single pragma Inline, however the Janus Ada compiler
   --  currently does not support that, which is why they are listed
   --  individually here
   --
   pragma Inline (Allocate);
   pragma Inline (Default_Subpool_For_Pool);
   pragma Inline (Initialize);
   pragma Inline (Finalize);
   pragma Inline (Is_Owner);
   pragma Inline (Set_Owner);

end Dynamic_Pools;
