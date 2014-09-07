------------------------------------------------------------------------------
--
--     Deepend - Dynamic Storage Pools for Ada 95, Ada 2005 and Ada 2012
--
--                 B O U N D E D   D Y N A M I C   P O O L S
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
--  ** NOTE: In the Ada 95 and Ada 2005 version of Dynamic_Pools, it is
--    erroneous to allocate objects that need finalization eg. (Tasks,
--    protected types, or objects of types inherited from types defined in
--    Ada.Finalization) and then deallocate the subpool associated with those
--    objects before they would have otherwise been finalized.
--
--  For Ada 2012, it is only erroneous to allocate task objects or objects
--  containing task components to a subpool.

with Ada.Task_Identification; use Ada.Task_Identification;

with System.Storage_Elements; use System;
with Sys.Storage_Pools.Subpools;
use Sys;

private with Ada.Finalization;

package Bounded_Dynamic_Pools is

   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   subtype Subpool_Handle is Storage_Pools.Subpools.Subpool_Handle;
   subtype Subpool_Count is Positive;

   Default_Subpool_Default_Size : constant := 16#FFFF#;
   --  The default size of the default subpool

   Default_Maximum_Subpool_Count : constant := 1_024;

   type Dynamic_Pool
     (Default_Subpool_Size : Storage_Elements.Storage_Count;
      Maximum_Subpools : Subpool_Count) is
     new Storage_Pools.Subpools.Root_Storage_Pool_With_Subpools
   with private;
   --  The Default_Subpool_Size is the size of the default
   --  subpool, and with the overriding Create_Subpool call. A value of zero
   --  implies that a default subpool is not needed and therefore is not
   --  created. However, in that case, the overriding Create_Subpool call will
   --  use the Default_Subpool_Default_Size value for the Subpool Size
   --  If a different subpool size is needed, there is another Create_Subpool
   --  variant that allows the subpool size to be specified.

   overriding
   function Create_Subpool
     (Pool : not null access Dynamic_Pool) return not null Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.
   --  Uses the Default_Block_Size of the Pool when more storage is needed,
   --  except if Pool.Default_Block_Size is zero, then the
   --  Default_Allocation_Block_Size value is used.

   not overriding
   function Create_Subpool
     (Pool : not null access Dynamic_Pool;
      Size : Storage_Elements.Storage_Count)
      return not null Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.

   overriding
   function Storage_Size
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of storage allocated from the pool
   --  and its subpools, including storage that is allocated but not used.

   function Storage_Size
     (Subpool : not null Subpool_Handle) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of storage allocated from the
   --  subpool, including storage that is allocated but not used.

   function Storage_Used
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of storage allocated to objects from the
   --  pool and its subpools. It assumes all currently filled blocks are fully
   --  allocated, but returns the exact amount for the current active block
   --  for each subpool.

   function Storage_Used
     (Subpool : not null Subpool_Handle) return Storage_Elements.Storage_Count;
   --  Indicates the current approximate amount of storage allocated to
   --  objects from the subpool. It assumes all currently filled blocks are
   --  fully allocated, but returns the exact amount for the current active
   --  block.

   function Is_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean;
   --  Returns True if the specified task "owns" the pool/subpool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Subpool : not null Subpool_Handle;
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

   overriding
   function Default_Subpool_For_Pool
     (Pool : not null access Dynamic_Pool) return not null Subpool_Handle;
   --  This calls returns the default subpool for the pool. It raises
   --  Storage_Error if Pool.Default_Block_Size is zero. The default
   --  subpool is used when Ada's "new" operator is used without specifying
   --  a subpool handle.

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Allocation
     (Subpool : Subpool_Handle) return Allocation_Type_Access;
   --  This generic routine provides a mechanism to allocate an object of
   --  a definite subtype from a specific subpool.

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Initialized_Allocation
     (Subpool : Subpool_Handle;
      Qualified_Expression : Allocation_Type) return Allocation_Type_Access;
   --  This generic routine provides a mechanism to allocate an object of
   --  a definite subtype from a specific subpool, and initializing the
   --  new object with a specific value.

   type Scoped_Subpool
     (Pool : not null access Dynamic_Pool;
      Size : Storage_Elements.Storage_Count;
      Heap_Allocated : Boolean) is tagged limited private;
   --  A Scoped subpool is a controlled type that manages its own internal
   --  subpool_handle that automatically deallocates the associated subpool
   --  when the Scoped_Subpool object is finalized. Typically, an object of
   --  this type is placed in a nested scope.

   function Handle
     (Subpool : Scoped_Subpool) return Subpool_Handle;

private

   use Ada;

   subtype Storage_Array is System.Storage_Elements.Storage_Array;

   type Dynamic_Subpool;
   type Dynamic_Subpool_Access is access all Dynamic_Subpool;

   type Subpool_Array is array (Positive range <>) of Dynamic_Subpool_Access;

   type Subpool_Vector (Size : Natural) is record
      Subpool_List : Subpool_Array (1 .. Size);
      Last : Natural := 0;
   end record;

   protected type Subpool_Set (Size : Natural) is

      procedure Initialize;
      procedure Add (Subpool : Dynamic_Subpool_Access);
      procedure Delete (Subpool : Dynamic_Subpool_Access);
      procedure Deallocate_All;
      function Storage_Total return Storage_Elements.Storage_Count;
      function Storage_Usage return Storage_Elements.Storage_Count;

   private
      Subpools : Subpool_Vector (Size);
      pragma Inline (Add);
   end Subpool_Set;

   subtype Storage_Array_Index is System.Storage_Elements.Storage_Offset range
     1 .. System.Storage_Elements.Storage_Offset'Last;

   type Dynamic_Subpool
     (Size : Storage_Elements.Storage_Count;
      Reusable : Boolean) is
     new Storage_Pools.Subpools.Root_Subpool with
      record
         Active : Storage_Array (1 .. Size);
         Next_Allocation : Storage_Array_Index;
         Owner : Ada.Task_Identification.Task_Id;
         Reclaimed : Boolean;
      end record;

   type Scoped_Subpool
     (Pool : not null access Dynamic_Pool;
      Size : Storage_Elements.Storage_Count;
      Heap_Allocated : Boolean) is new Finalization.Limited_Controlled with
      record
         Handle : Subpool_Handle;

         case Heap_Allocated is
            when True =>
               null;

            when False =>
               Storage : aliased Dynamic_Subpool
                 (Size => Size, Reusable => True);
         end case;
      end record;

   overriding
   procedure Initialize (Subpool : in out Scoped_Subpool);

   overriding
   procedure Finalize (Subpool : in out Scoped_Subpool);

   type Dynamic_Pool
     (Default_Subpool_Size : Storage_Elements.Storage_Count;
      Maximum_Subpools : Subpool_Count)
     is new Storage_Pools.Subpools.Root_Storage_Pool_With_Subpools
   with record
      Default_Subpool : Subpool_Handle;
      Subpools : Subpool_Set (Maximum_Subpools);
   end record;

   use type Storage_Elements.Storage_Count;

   overriding
   procedure Allocate
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding
   procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : not null Subpool_Handle);
   --  with Pre => Is_Owner (Subpool, Current_Task);

   overriding
   procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool;
      Subpool : in out Subpool_Handle);
   --  Deallocate the space for all of the objects allocated from the
   --  specified subpool, and destroy the subpool. The subpool handle
   --  is set to null after this call.

   overriding
   procedure Initialize (Pool : in out Dynamic_Pool);
   --  Create the default subpool if Pool.Default_Block_Size is non-zero

   overriding
   procedure Finalize   (Pool : in out Dynamic_Pool);

   pragma Inline
     (Allocate, Default_Subpool_for_Pool,
      Initialize, Finalize, Is_Owner, Set_Owner);

end Bounded_Dynamic_Pools;
