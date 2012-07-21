------------------------------------------------------------------------------
--
--              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
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

--  Deepend provides storage pools with subpool capabilities for Ada 2005
--  and Ada 2012. Subpools provide an efficient mechanism to release
--  storage that is considered to be safer and more efficient than
--  other strategies such as garbage collection. In Ada 2012, the new
--  allocation syntax may be used with this pool, to specify the subpool that
--  will contain the allocated objects.
--
--     e.g.
--          Object := new (subpool_name) Object_Type'(Value);
--
--  For Ada 2005, the same effect can be obtained by using the Allocation
--  and Initialized_Allocation generics provided by this package. These
--  generics allow allocating non-controlled objects of definite types to a
--  particular subpool.
--  In addition, for both Ada 2005 and Ada 2012, the new operator may be used
--  without specifying a subpool, which results in an object being allocated
--  to the default subpool for the storage pool.
--
--  A Dynamic_Pool allows objects allocated from the pool to be reclaimed
--  all at once, instead of requiring each object to be individually
--  reclaimed one at a time via the Ada.Unchecked_Deallocation generic.
--  In fact, Ada.Unchecked_Deallocation is not needed or expected to be used
--  with this storage pool.
--
--  Tasks can create subpools from the same Dynamic Pool object at the
--  same time, but only one task may allocate from a specific subpool
--  instance at a time. A task "owns" its subpools, and attempts by other
--  tasks to allocate from subpool owned by another task results in
--  a Program_Error exception being raised.
--
--  There is both an Ada 2005 version of the packages and an Ada 2012 version.
--  The Ada 2005 version was designed to be compatible with the Ada 2012
--  version as much as possible, although the Ada 2012 version does take
--  advantage of the new features of the language, including the new subpool
--  facility provided by System.Storage_Pools.Subpools.
--
--  Also, the 2012 version eliminates access parameters in favor of in out
--  parameters for function calls, and allows defaults for discriminated types.
--  The Ada 2005 version of the Dynamic_Pools package does not support
--  allocations of unconstrained types, or objects that need finalization such
--  as controlled types, or protected types, although there is a Deepend
--  package, Basic_Dynamic_Pools, that does provide these capabilities in
--  Ada 2005, though that package does not provide subpool capabilities.
--
--  The Ada 2012 Dynamic_Pools package provides both subpool capabilities and
--  the ability to allocate controlled types and unconstrained types.
--  Calling Unchecked_Deallocate_Subpool will finalize these objects and
--  release any other storage associated with the subpool.
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
with Ada.Finalization;
with Ada.Unchecked_Deallocate_Subpool;

with System.Storage_Elements; use System;
with System.Storage_Pools.Subpools;

private with Ada.Containers.Vectors;

package Dynamic_Pools is

   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   Ada2012_Warnings : constant Boolean := False;
   --  Set to true to generate compiler warnings about changes still
   --  needed for Ada 2012

   --  pragma Preelaborate;
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "Should Ada.U_D_S have pragma Preelaborate?");

   subtype Subpool_Handle is Storage_Pools.Subpools.Subpool_Handle;

   package Scoped_Subpools is
      --  Scoped subpools define a controlled object that wraps a subpool
      --  handle, that automatically deallocates the subpool when the
      --  Scoped_Subpool_Handle object is finalized. Typically, the
      --  Create_Subpool call returning this type will be used to place an
      --  object in a nested scope.

      type Scoped_Subpool_Handle (Handle : Subpool_Handle) is new
        Ada.Finalization.Limited_Controlled with null record;
       --  Calls Unchecked_Deallocate_Subpool during finalization

   private

      overriding
       procedure Finalize (Scoped_Subpool : in out Scoped_Subpool_Handle);

   end Scoped_Subpools;

   subtype Scoped_Subpool_Handle is Scoped_Subpools.Scoped_Subpool_Handle;

   Default_Allocation_Block_Size : constant := 16#FFFF#;
   --  A Block Size is the size of the heap allocation used when more
   --  storage is needed for a subpool. Larger block sizes imply less
   --  heap allocations. Generally, better performance involves using larger
   --  block sizes.

   type Dynamic_Pool
     (Default_Block_Size : Storage_Elements.Storage_Count :=
        Default_Allocation_Block_Size) is
     new Storage_Pools.Subpools.Root_Storage_Pool_With_Subpools
   with private;
   --  The Default_Block_Size is the Block Size associated with the default
   --  subpool, and with the overriding Create_Subpool call. A value of zero
   --  implies that a default subpool is not needed and therefore is not
   --  created. However, in that case, the overriding Create_Subpool call will
   --  use the Default_Allocation_Block_Size value for the Block Size

   overriding
   function Create_Subpool
     (Pool : in out Dynamic_Pool) return not null Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.
   --  Uses the Default_Block_Size of the Pool when more storage is needed,
   --  except if Pool.Default_Block_Size is zero, then the
   --  Default_Allocation_Block_Size value is used.

   not overriding
   function Create_Subpool
     (Pool : in out Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count)
      return not null Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.

   not overriding
   function Create_Subpool
     (Pool : in out Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count :=
        Default_Allocation_Block_Size) return Scoped_Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.

   overriding function Storage_Size
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of memory allocated from the pool
   --  and its subpools, including storage that is allocated but not used.

   function Storage_Size
     (Subpool : not null Subpool_Handle) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of memory allocated from the
   --  subpool, including storage that is allocated but not used.

   function Storage_Used
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of memory allocated from the pool
   --  and its subpools. It assumes all currently filled blocks are fully
   --  allocated, but returns the exact amount for the current active block
   --  for each subpool.

   function Storage_Used
     (Subpool : not null Subpool_Handle) return Storage_Elements.Storage_Count;
   --  Indicates the current approximate amount of memory allocated from the
   --  subpool. It assumes all currently filled blocks are fully allocated,
   --  but returns the exact amount for the current active block.

   function Is_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean;
   --  Returns True if the specified task "owns" the pool/subpool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task)
   with Pre => (Is_Owner (Subpool, Null_Task_Id) and then T = Current_Task)
                or else (Is_Owner (Subpool) and then T = Null_Task_Id),
        Post => Is_Owner (Subpool, T);
   --  An Owning task can relinquish ownership of a subpool by setting the
   --  owner to a Null_Task_Id. Another task may obtain ownership of a subpool,
   --  provided that the subpool has no owner.

   procedure Unchecked_Deallocate_Subpool
     (Subpool : in out Subpool_Handle)
      renames Ada.Unchecked_Deallocate_Subpool;
   --  This call performs unchecked storage deallocation of all objects
   --  allocated from the subpool, then destroys the subpool, setting
   --  Subpool to null.

   overriding
   function Default_Subpool_For_Pool
     (Pool : Dynamic_Pool) return not null Subpool_Handle;
   --  This calls returns the default subpool for the pool. It raises
   --  Storage_Error if Pool.Default_Block_Size is zero. The default
   --  subpool is used when Ada's "new" operator is used.

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Allocation
     (Subpool : Subpool_Handle) return Allocation_Type_Access;
   --  This generic routine provides a mechanism to allocate an object of
   --  a definite subtype from a specific subpool.
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
     "These generics currently have an edge in performance over using the " &
     "new Ada 2012 allocator syntax, otherwise they shouldn't be needed");

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Initialized_Allocation
     (Subpool : Subpool_Handle;
      Qualified_Expression : Allocation_Type) return Allocation_Type_Access;
   --  This generic routine provides a mechanism to allocate an object of
   --  a definite subtype from a specific subpool, and initializing the
   --  new object with a specific value.
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
     "These generics currently have an edge in performance over using the " &
     "new Ada 2012 allocator syntax, otherwise they shouldn't be needed");

private

   subtype Storage_Array is System.Storage_Elements.Storage_Array
   with Static_Predicate => Storage_Array'First = 1;

   type Storage_Array_Access is access Storage_Array;

   pragma Warnings (Off, "*Warnings Off*could be omitted*");

   package Storage_Vector is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Storage_Array_Access);

   type Dynamic_Subpool;
   type Dynamic_Subpool_Access is access all Dynamic_Subpool;

   package Subpool_Vector is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Dynamic_Subpool_Access);

   pragma Warnings (On, "*Warnings Off*could be omitted*");

   protected type Subpool_Set is

      procedure Add (Subpool : Dynamic_Subpool_Access);
      procedure Delete (Subpool : in out Dynamic_Subpool_Access);
      function Storage_Usage return Storage_Elements.Storage_Count;
      function Storage_Total return Storage_Elements.Storage_Count;
      function Get_Subpools_For_Finalization  return Subpool_Vector.Vector;

   private
      Subpools : Subpool_Vector.Vector;
      pragma Inline (Add);
   end Subpool_Set;

   subtype Storage_Array_Index is System.Storage_Elements.Storage_Offset
   with Static_Predicate => Storage_Array_Index >= 1;

   type Dynamic_Subpool
     (Block_Size : Storage_Elements.Storage_Count) is
     new Storage_Pools.Subpools.Root_Subpool with
      record
         Used_List : Storage_Vector.Vector;
         Free_List : Storage_Vector.Vector;
         Active : Storage_Array_Access;
         Next_Allocation : Storage_Array_Index;
         Owner : Ada.Task_Identification.Task_Id;
      end record;

   type Dynamic_Pool
     (Default_Block_Size : Storage_Elements.Storage_Count :=
        Default_Allocation_Block_Size)
     is new Storage_Pools.Subpools.Root_Storage_Pool_With_Subpools
   with record
      Default_Subpool : Subpool_Handle;
      Subpools : Subpool_Set;
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
      Subpool : not null Subpool_Handle)
   with Pre => Is_Owner (Subpool, Current_Task);

--   We want Allocate_From_Subpool to be fast. The precondition
--   is supposed to hold true, but not sure whether we want to enable the
--   precondition, if it impacts performance. Preconditions can be disabled
--   however, by setting the Assertion_Policy to IGNORE, (or by setting
--   Assertion_Policy (Pre => IGNORE) )

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

   overriding
   function Default_Subpool_For_Pool
     (Pool : Dynamic_Pool) return not null Subpool_Handle
   is (Pool.Default_Subpool);

   overriding function Storage_Size
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count
   is (Pool.Subpools.Storage_Total);

   function Storage_Used
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count
   is (Pool.Subpools.Storage_Usage);

   pragma Inline
     (Allocate, Default_Subpool_for_Pool,
      Initialize, Finalize, Is_Owner, Set_Owner);

end Dynamic_Pools;
