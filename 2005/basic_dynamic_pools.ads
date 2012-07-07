------------------------------------------------------------------------------
--
--              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
--
--                   B A S I C   D Y N A M I C   P O O L S
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

--  A Basic_Dynamic_Pool is a storage pool written for both Ada 2005
--  and Ada 2012 that does not provide subpool capabilities. This provides
--  a simpler strategy if subpool capabilities are not needed, and should
--  provide the best performance, owing to the simpler strategy chosen.
--
--  A Basic_Dynamic_Pool allows objects allocated from the pool to be reclaimed
--  all at once, instead of requiring each object to be individually
--  reclaimed one at a time via the Ada.Unchecked_Deallocation generic.
--  In fact, Ada.Unchecked_Deallocation is not needed or expected to be used
--  with this storage pool.
--
--  Only one task may allocate from a specific Basic_Dynamic_Pool instance at
--  a time. Attempts by other tasks to allocate from a pool owned by another
--  task results in a Program_Error exception being raised.
--
--  A Basic_Dynamic_Pool may be used to allocate any object in Ada 2005
--  or Ada 2012.
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
--  All allocations from a Basic_Dynamic_Pool are made via Ada's "new"
--  operator, as defined in Ada 2005.

with Ada.Task_Identification; use Ada.Task_Identification;
with System.Storage_Elements; use System;
with System.Storage_Pools;

private with Ada.Containers.Vectors;

package Basic_Dynamic_Pools is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   Default_Allocation_Block_Size : constant := 16#FFFF#;
   --  A Block Size is the size of the heap allocation used when more
   --  storage is needed for the pool. Larger block sizes imply less
   --  heap allocations. Generally, better performance involves using larger
   --  block sizes.

   type Basic_Dynamic_Pool
     (Block_Size : Storage_Elements.Storage_Count)
     is new Storage_Pools.Root_Storage_Pool with private;
   --  The Block_Size specifies how much memory is allocated from the heap
   --  when the storage pool needs more storage.

   overriding function Storage_Size
     (Pool : Basic_Dynamic_Pool) return Storage_Elements.Storage_Count;
   --  Indicates the current amount of memory allocated from the pool.
   --  It assumes all currently filled blocks are fully allocated, but
   --  returns the exact amount for the current active block.

   function Is_Owner
     (Pool : Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean;
   --  Returns True if the specified task "owns" the pool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Pool : in out Basic_Dynamic_Pool;
      T : Task_Id := Current_Task);
   --  An Owning task can relinquish ownership of a pool by setting the
   --  owner to a Null_Task_Id. Another task may obtain ownership of a pool,
   --  provided that the pool has no owner.

private

   type Storage_Array_Access is access System.Storage_Elements.Storage_Array;

   pragma Warnings (Off, "*Warnings Off*could be omitted");
   package Storage_Vector is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Storage_Array_Access);
   pragma Warnings (On, "*Warnings Off*could be omitted");

   subtype Storage_Array_Index is System.Storage_Elements.Storage_Offset range
     1 .. System.Storage_Elements.Storage_Offset'Last;

   type Basic_Dynamic_Pool
     (Block_Size : Storage_Elements.Storage_Count)
     is new Storage_Pools.Root_Storage_Pool with
      record
         Used_List : Storage_Vector.Vector;
         Free_List : Storage_Vector.Vector;
         Active : Storage_Array_Access;
         Next_Allocation : Storage_Array_Index;
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
   procedure Initialize (Pool : in out Basic_Dynamic_Pool);

   overriding
   procedure Finalize   (Pool : in out Basic_Dynamic_Pool);

   pragma Inline
     (Allocate,
      Initialize, Finalize, Is_Owner, Set_Owner);

end Basic_Dynamic_Pools;
