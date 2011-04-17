------------------------------------------------------------------------------
--                                                                          --
--                   Deepend - Dynamic Pools for Ada 2005                   --
--                                                                          --
--                   B A S I C   D Y N A M I C   P O O L S                         --
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

--  Deepend is a storage pools written in Ada 2005 that is intended to be
--  compatible as much as possible with the Ada 2012 proposal for creating
--  storage subpools. Once Ada 2012 becomes available, Deepend will be
--  updated to be fully compliant with Ada 2012 Subpools. The only
--  significant incompatible changes to the interface will be involving
--  function calls that have in out parameters instead of access parameters,
--  and likely the elimination of the Allocation and Initialized_Allocation
--  generics, since they will no longer be needed.
--
--  A Deepend subpool allows objects allocated from the pool to be reclaimed
--  all at once, instead of requiring each object to be individually
--  reclaimed one at a time via the Ada.Unchecked_Deallocation generic.
--  In fact, Ada.Unchecked_Deallocation is not needed or expected to be used
--  with this storage pool. The allocations from a Deepend subpool may be
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

with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Finalization;
with System.Storage_Elements; use System;
with System.Storage_Pools;

private with Ada.Containers.Vectors;

package Basic_Dynamic_Pools is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   type Basic_Dynamic_Pool
     (Block_Size : Storage_Elements.Storage_Count)
     is new Storage_Pools.Root_Storage_Pool with private;
   --  The Default_Block_Size is the Block Size associated with the default
   --  subpool, and with the overriding Create_Subpool call. A value of zero
   --  implies that a default subpool is not needed and therefore is not
   --  created. However, in that case, the overriding Create_Subpool call will
   --  use the Default_Allocation_Block_Size value for the Block Size

   function Is_Owner
     (Pool : Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean;
   --  Returns True if the specified task "owns" the pool/subpool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Pool : in out Basic_Dynamic_Pool;
      T : Task_Id := Current_Task);
   pragma Precondition
     ((Is_Owner (Pool, Null_Task_Id) and then T = Current_Task)
      or else (Is_Owner (Pool) and then T = Null_Task_Id));
   pragma Postcondition (Is_Owner (Pool, T));
   --  An Owning task can relinquish ownership of a subpool by setting the
   --  owner to a Null_Task_Id. Another task may obtain ownership of a subpool,
   --  provided that the subpool has no owner.

   procedure Unchecked_Deallocate_Objects
     (Pool : in out Basic_Dynamic_Pool);
   --  This call performs unchecked storage deallocation of all objects
   --  allocated from the subpool. The subpool itself is not destroyed by this
   --  call. The intent is to allow new objects to be allocated from the
   --  subpool after this call, which should be more efficient than calling
   --  Unchecked_Deallocate_Subpool, and then having to call Create_Subpool
   --  to create a new subpool.

private

   type Storage_Array_Access is access System.Storage_Elements.Storage_Array;

   package Storage_Vector is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Storage_Array_Access);

   type Basic_Dynamic_Pool (Block_Size : Storage_Elements.Storage_Count)
     is new Storage_Pools.Root_Storage_Pool with
      record
         Used_List : Storage_Vector.Vector;
         Free_List : Storage_Vector.Vector;
         Active : Storage_Array_Access;
         Next_Allocation : System.Storage_Elements.Storage_Offset;
         Owner : Ada.Task_Identification.Task_Id;
      end record;

   overriding
   procedure Allocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count);

   overriding
   procedure Deallocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is null;

   overriding
   function Storage_Size
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count;

   overriding
   procedure Initialize (Pool : in out Basic_Dynamic_Pool);

   overriding
   procedure Finalize   (Pool : in out Basic_Dynamic_Pool);

   pragma Inline
     (Unchecked_Deallocate_Objects, Allocate,
      Initialize, Finalize, Is_Owner, Set_Owner);

end Basic_Dynamic_Pools;
