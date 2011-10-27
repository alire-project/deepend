------------------------------------------------------------------------------
--                                                                          --
--                   Deepend - Dynamic Pools for Ada 2005                   --
--                                                                          --
--                        D Y N A M I C   P O O L S                         --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore                    --
--                                                                          --
--  Deepend is free software;  you can  redistribute it  and/or modify it  --
--  under  terms of the  GNU General Public License  as  published  by the  --
--  Free Software  Foundation;  either version 2,  or (at your option) any  --
--  later  version.  Paraffin is  distributed in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU  --
--  General Public License for  more details.  You should have  received a  --
--  copy of the GNU General Public License distributed with Deepend;  see   --
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,   --
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.         --
--                                                                          --
--  As a  special exception, if other files  instantiate generics from      --
--  this unit,  or you link this  unit with other files  to produce an      --
--  executable,  this unit  does  not by  itself  cause the  resulting      --
--  executable to be covered by  the GNU General Public License.  This      --
--  exception does  not however invalidate  any other reasons  why the      --
--  executable file might be covered by the GNU Public License.             --
------------------------------------------------------------------------------

--  Deepend provides storage pools written in Ada 2005 that are intended to be
--  compatible as much as possible with the Ada 2012 proposal for creating
--  storage subpools. Once Ada 2012 becomes available, Deepend will be
--  updated to be fully compliant with Ada 2012 Subpools. The only
--  significant incompatible changes to the interface will be involving
--  function calls that have in out parameters instead of access parameters,
--  and likely the elimination of the Allocation and Initialized_Allocation
--  generics, since they will no longer be needed.
--
--  A Dynamic_Pool allows objects allocated from the pool to be reclaimed
--  all at once, instead of requiring each object to be individually
--  reclaimed one at a time via the Ada.Unchecked_Deallocation generic.
--  In fact, Ada.Unchecked_Deallocation is not needed or expected to be used
--  with this storage pool. The allocations from a Dynamic_Pool subpool may be
--  reclaimed multiple times before the end of the subpool's lifetime
--  through the explicit call, Unchecked_Deallocate_Objects.
--
--  Tasks can create subpools from the same Dynamic Pool object at the
--  same time, but only one task may allocate from a specific subpool
--  instance at a time. A task "owns" its subpools, and attempts by other
--  tasks to allocate from subpool owned by another task results in
--  a Program_Error exception being raised.
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
--  ** NOTE: It is erroneous to allocate objects that need finalization
--  eg. (Tasks, or objects of types inherited from types defined in
--       Ada.Finalization)
--  and then Release the storage associated with those objects before
--  they would have otherwise been finalized.
--  Once Ada 2012 becomes available, it will be possible to allocate
--  objects of controlled types needing finalization. Currently Ada 2012
--  is proposing to disallow task allocations from subpools however.
--
--  Also, the interface provided here does not allow allocating unconstrained
--  objects, except by using Ada's new operator. In that case, you cannot
--  allocate from a specific subpool, but each Dynamic_Pool object has a
--  default subpool. Using the new operator allocates from this default
--  subpool. Ada 2012 is proposing to provide new syntax for the new operator
--  that will allow allocating from a specific subpool. This will eliminate
--  the need for the allocation generics in this package, as well as provide
--  a means to allocate uncontrained objects from specific subpools.

with Sys.Storage_Pools.Subpools; use Sys.Storage_Pools.Subpools; use Sys;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Finalization;
with System.Storage_Elements; use System;

private with Ada.Containers.Vectors;

package Dynamic_Pools is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   Ada2012_Warnings : constant Boolean := False;
   --  Set to true to generate compiler warnings about changes needed
   --  for Ada 2012

   package Scoped_Subpools is
      --  Scoped subpools define an controlled object that wraps a subpool
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

   subtype Subpool_Handle is Storage_Pools.Subpools.Subpool_Handle;
   subtype Scoped_Subpool_Handle is Scoped_Subpools.Scoped_Subpool_Handle;

   Default_Allocation_Block_Size : constant := 16#FFFF#;
   --  A Block Size is the size of the heap allocation used when more
   --  storage is needed for a subpool. Larger block sizes imply less
   --  heap allocations. Generally, better performance involves using larger
   --  block sizes.

   type Dynamic_Pool (Default_Block_Size : Storage_Elements.Storage_Count) is
     new Storage_Pools.Subpools.Root_Storage_Pool_with_Subpools
   with private;
   --  The Default_Block_Size is the Block Size associated with the default
   --  subpool, and with the overriding Create_Subpool call. A value of zero
   --  implies that a default subpool is not needed and therefore is not
   --  created. However, in that case, the overriding Create_Subpool call will
   --  use the Default_Allocation_Block_Size value for the Block Size
   pragma Compile_Time_Warning
     (Ada_2012_Warnings, "In Ada 2012, " &
      "use Default_Allocation_Block_Size as default discriminant");

   overriding
   function Create_Subpool
     (Pool : access Dynamic_Pool) return not null Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.
   --  Uses the Default_Block_Size of the Pool when more storage is needed,
   --  except if Pool.Default_Block_Size is zero, then the
   --  Default_Allocation_Block_Size value is used.

   not overriding
   function Create_Subpool
     (Pool : access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count)
      return not null Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.

   function Create_Subpool
     (Pool : access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count :=
        Default_Allocation_Block_Size) return Scoped_Subpool_Handle;
   --  The task calling Create_Subpool initially "owns" the subpool.

   function Is_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean;
   --  Returns True if the specified task "owns" the pool/subpool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task);
   pragma Precondition
     ((Is_Owner (Subpool, Null_Task_Id) and then T = Current_Task)
      or else (Is_Owner (Subpool) and then T = Null_Task_Id));
   pragma Postcondition (Is_Owner (Subpool, T));
   --  An Owning task can relinquish ownership of a subpool by setting the
   --  owner to a Null_Task_Id. Another task may obtain ownership of a subpool,
   --  provided that the subpool has no owner.

   procedure Unchecked_Deallocate_Objects
     (Subpool : Subpool_Handle);
   --  This call performs unchecked storage deallocation of all objects
   --  allocated from the subpool. The subpool itself is not destroyed by this
   --  call. The intent is to allow new objects to be allocated from the
   --  subpool after this call, which should be more efficient than calling
   --  Unchecked_Deallocate_Subpool, and then having to call Create_Subpool
   --  to create a new subpool.

   procedure Unchecked_Deallocate_Subpool
     (Subpool : in out Subpool_Handle);
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "For Ada 2012, rename procedure to Ada.U_D_S");
   --  This call performs unchecked storage deallocation of all objects
   --  allocated from the subpool, then destroys the subpool, setting
   --  Subpool to null.

   overriding
   function Default_Subpool_for_Pool
     (Pool : Dynamic_Pool) return not null Subpool_Handle;
   --  This calls returns the default subpool for the pool. It raises
   --  Storage_Error if Pool.Default_Block_Size is zero. The default
   --  subpool is used when Ada's "new" operator is used.

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Allocation
     (Subpool : Subpool_Handle) return Allocation_Type_Access;
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "For Ada 2012, eliminate this generic");
   --  This generic routine provides a mechanism to allocate an object of
   --  a definite subtype from a specific subpool.

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Initialized_Allocation
     (Subpool : Subpool_Handle;
      Qualified_Expression : Allocation_Type) return Allocation_Type_Access;
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "For Ada 2012, eliminate this generic");
   --  This generic routine provides a mechanism to allocate an object of
   --  a definite subtype from a specific subpool, and initializing the
   --  new object with a specific value.

private

   type Storage_Array_Access is access System.Storage_Elements.Storage_Array;

   package Storage_Vector is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Storage_Array_Access);

   type Dynamic_Subpool;
   type Dynamic_Subpool_Access is access all Dynamic_Subpool;

   package Subpool_Vector is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Dynamic_Subpool_Access);

   protected type Subpool_Set is

      procedure Add (Subpool : Dynamic_Subpool_Access);
      procedure Delete (Subpool : Dynamic_Subpool_Access);
      procedure Deallocate_All;

   private
      Subpools : Subpool_Vector.Vector;
      pragma Inline (Add);
   end Subpool_Set;

   type Dynamic_Subpool
     (Block_Size : Storage_Elements.Storage_Count) is new Root_Subpool with
      record
         Used_List : Storage_Vector.Vector;
         Free_List : Storage_Vector.Vector;
         Active : Storage_Array_Access;
         Next_Allocation : System.Storage_Elements.Storage_Offset;
         Owner : Ada.Task_Identification.Task_Id;
         Deallocate_Storage : Boolean;
      end record;

   type Dynamic_Pool (Default_Block_Size : Storage_Elements.Storage_Count) is
     new Root_Storage_Pool_with_Subpools with
      record
         Default_Subpool : Subpool_Handle;
         Subpools : Subpool_Set;
      end record;

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
      Subpool : -- not null
      Subpool_Handle);
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "GNAT Ada 2005 compiler bug, can't use not null here");
--   pragma Precondition
--     (Is_Owner (Subpool, Current_Task));
--   We want Allocate_From_Subpool to be fast. The commented out precondition
--   is supposed to hold true, but not sure whether we want to enable the
--   precondition, if it impacts performance.

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
     (Unchecked_Deallocate_Objects, Allocate, Default_Subpool_for_Pool,
      Initialize, Finalize, Is_Owner, Set_Owner);

end Dynamic_Pools;
