--  NOTES: 1) subpools can have subpools
--         2) Can deallocate objects from pool without releasing pools storage
--         3) pools can be independant for the same access type

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
--  copy of the GNU General Public License distributed with Paraffin;  see  --
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

--  Deepend provides a framework for creating storage pools in Ada 2005 where
--  objects allocated from the pool can be reclaimed all at once, instead of
--  requiring each object to be individually reclaimed one at time via the
--  Ada.Unchecked_Deallocation generic. In fact, Ada.Unchecked_Deallocation is
--  not needed or expected to be used with this family of storage pools, which
--  is why Deallocate is overriden with a null procedure. Dynamic pools also
--  provide a mechanism to allow allocations of the same access type to be
--  allocated from different pool objects. Deepend provides an implementation
--  of dynamic pools that provides subpool capabilities. These pool objects may
--  be completely independent pools, or they may be chained together in order
--  to extend the lifetime of child pools to that of the lifetime of the root
--  parent pool. In addition, the pool may be reclaimed multiple times before
--  the end of its lifetime through an explicit call.
--
--  Deepend currently provides a binding to the Apache Runtime
--  Pools Implementation. This may change in the future, as it is desirable to
--  provide a solution written entirely in Ada.

--  with System.Storage_Elements;
with Sys.Storage_Pools.Subpools; use Sys.Storage_Pools.Subpools; use Sys;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Finalization;
with System.Storage_Elements;

private with Ada.Containers.Vectors;

package Dynamic_Pools is
   pragma Elaborate_Body;

   Ada2012_Warnings : constant Boolean := False;

   --  Needed to ensure that library routines can execute allocators

   --  Allocation strategy:

   --    Deallocate is not needed or used, and is implemented as a null
   --    procedure. Use of this storage pool means that there is no need for
   --    calls to Ada.Unchecked_Deallocation.
   --
   --  ** NOTE: It is erroneous to allocate objects that need finalization
   --  eg. (Tasks, or objects of types inherited from types defined in
   --       Ada.Finalization)
   --  and then Release the storage associated with those objects before
   --  they would have otherwise been finalized. Ada 2012 is proposing to
   --  provide facilities that would allow such early finalization.

   subtype Subpool_Handle is Storage_Pools.Subpools.Subpool_Handle;

   type Dynamic_Pool
     (Minimum_Allocation : System.Storage_Elements.Storage_Count) is
     new Storage_Pools.Subpools.Root_Storage_Pool_with_Subpools
   with private;
   pragma Compile_Time_Warning
     (Ada_2012_Warnings, "In Ada 2012, use 4096 as default discriminant");

   function Create_Subpool
     (Pool : access Dynamic_Pool) return not null Subpool_Handle;

   function Is_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean;
   --  Returns True if the specified task owns the pool/subpool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task);
   pragma Precondition
     ((Is_Owner (Subpool, Null_Task_Id) and then T = Current_Task)
      or else (Is_Owner (Subpool) and then T = Null_Task_Id));
   pragma Postcondition (Is_Owner (Subpool, T));
   --  The task that owns a pool/subpool and therefore allowed to allocate
   --  from it, can only be specified once for a particular pool.

   procedure Unchecked_Deallocate_Objects
     (Subpool : Subpool_Handle);
   pragma Postcondition
     (not Objects_Need_Finalization (Subpool));
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "For Ada 2012, this should be Post'Class");
   --  This call performs unchecked storage deallocation of all objects
   --  allocated from the pool. After this call, the Pool may still need
   --  finalization of its storage. The intent is to allow new objects to be
   --  allocated from the Pool after this call, but possibly eliminating the
   --  overhead of deallocating the storage and then having to allocate new
   --  storage for the pool for subsequent allocations. This allows the
   --  possibility of existing storage to be reused. Derivations of the
   --  Dynamic_Pool type must call the procedure of this abstract type from
   --  overriding procedures. (This is enforced by the postcondition.)

   procedure Unchecked_Deallocate_Subpool
     (Subpool : in out Subpool_Handle);
   pragma Postcondition
     (not Objects_Need_Finalization (Subpool));

   pragma Compile_Time_Warning
     (Ada2012_Warnings, "For Ada 2012, use post");
   --  This call must have the effect of calling Unchecked_Deallocate_Objects
   --  for the specified Pool, and then releases all resources associated with
   --  the pools storage to the system, if possible. Whether or not new
   --  allocations can be made from the pool after this call is determined
   --  by the implementation of the derived type. If such allocations are
   --  allowed, then the allocations likely, though not necessarily, would
   --  involve new storage being to be allocated to the pool.

   function Objects_Need_Finalization
     (Subpool : Subpool_Handle) return Boolean;
   --  Returns true if there are objects allocated from the pool that have
   --  not been deallocated and need finalization.

   overriding
   function Default_Subpool_for_Pool
     (Pool : Dynamic_Pool) return not null Subpool_Handle;

   type Scope_Bomb (Subpool : Subpool_Handle) is new
     Ada.Finalization.Limited_Controlled with private;
   --  Calls Unchecked_Deallocate_Subpool during finalization

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Allocation
     (Subpool : Subpool_Handle) return Allocation_Type_Access;
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "For Ada 2012, eliminate this generic");
   --  This generic routine provides a mechanism to allocate an object of
   --  a definite subtype from a pool.
   --  The "new" has to be associated with the root storage pool, and currently
   --  there is no way to override the storage pool object for the "new"
   --  operator.
   --
   --  This function allows the storage pool object to be specified for an
   --  allocation.

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Initialized_Allocation
     (Subpool : Subpool_Handle;
      Qualified_Expression : Allocation_Type) return Allocation_Type_Access;
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "For Ada 2012, eliminate this generic");
   --  This generic routine provides a mechanism to allow an object of an
   --  indefinite subtype, or a qualified expression from a pool.
   --  The "new" has to be associated with the root storage pool, and currently
   --  there is no way to override the storage pool object for the "new"
   --  operator.
   --
   --  This function allows the storage pool object to be specified for an
   --  allocation.

private

   use System;

   type Storage_Array_Access is access System.Storage_Elements.Storage_Array;

   package Storage_Vector is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Storage_Array_Access);

   type Dynamic_Subpool;
   type Dynamic_Subpool_Access is access all Dynamic_Subpool;
--   type Access_To_Subpool_Access is access all Dynamic_Subpool_Access;

   package Subpool_Vector is new
     Ada.Containers.Vectors (Index_Type => Positive,
                             Element_Type => Dynamic_Subpool_Access);

   protected type Subpool_Set is
      procedure Add (Subpool : Dynamic_Subpool_Access);
      procedure Delete (Subpool : Dynamic_Subpool_Access);
      procedure Deallocate_All;
      function Get_Default_Subpool return Subpool_Handle;
   private
      Subpools : Subpool_Vector.Vector;
   end Subpool_Set;

   type Dynamic_Subpool is new Root_Subpool with
      record
         Used_List : Storage_Vector.Vector;
         Free_List : Storage_Vector.Vector;
         Active : Storage_Array_Access;
         Next_Allocation : System.Storage_Elements.Storage_Offset;
         Owner : Ada.Task_Identification.Task_Id;
         Deallocate_Storage : Boolean;
      end record;

   type Dynamic_Pool
     (Minimum_Allocation : System.Storage_Elements.Storage_Count) is
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
   pragma Precondition
     (Is_Owner (Subpool, Current_Task));

   overriding procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool;
      Subpool : in out Subpool_Handle);
   --  with Pre'Class => Pool_of_Subpool(Subpool) = Pool'access
   --  Deallocate the space for all of the objects allocated from the
   --  specified subpool, and destroy the subpool. The subpool handle
   --  is set to null after this call.

   overriding procedure Initialize (Pool : in out Dynamic_Pool);

   overriding procedure Finalize   (Pool : in out Dynamic_Pool);

   type Scope_Bomb (Subpool : Subpool_Handle) is new
     Ada.Finalization.Limited_Controlled with null record;

   overriding procedure Finalize   (Scope : in out Scope_Bomb);

   pragma Inline
     (Unchecked_Deallocate_Objects,
      Objects_Need_Finalization);

end Dynamic_Pools;
