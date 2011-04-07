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

with System.Storage_Elements;
with Sys.Storage_Pools.Subpools;

package Dynamic_Pools is
   pragma Elaborate_Body;

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

   Ada2012_Warnings : Boolean renames
     Sys.Storage_Pools.Subpools.Ada2012_Warnings;

   use type System.Address;

   type Dynamic_Pool is abstract new
     Sys.Storage_Pools.Subpools.Root_Storage_Pool_with_Subpools with private;

   type Dynamic_Subpool is abstract new
     Sys.Storage_Pools.Subpools.Root_Subpool with private;

   type Dynamic_Subpool_Handle is abstract new
     Sys.Storage_Pools.Subpools.Subpool_Handle with private;

   overriding procedure Deallocate
     (Pool         : in out Dynamic_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count)
   is null;
   --  Ada.Unchecked_Deallocation is not needed for this family of storage
   --  pools. Deallocate is not meant to be called (directly or indirectly in
   --  the usual way via Ada.Unchecked_Deallocation, as this call has no effect
   --  for a Dynamic Pool. Deallocation of objects in the pool occurs when the
   --  Storage pool object is finalized (or when Unchecked_Deallocate_Objects
   --  or Unchecked_Deallocate_Storage is called).

   --  BJM: Ada 2012 currently will not support providing access to
   --  finalize objects in a pool, it only provides access to finalize objects
   --  in a subpool.

   procedure Unchecked_Deallocate_Objects
     (Pool : in out Dynamic_Pool) is abstract;
   --  pragma Postcondition (not Has_Allocations (Pool));
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

   procedure Unchecked_Deallocate_Objects
     (SubPool : in out Dynamic_Subpool_Handle) is abstract;
   --  pragma Postcondition (not Has_Allocations (Subpool));
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

   procedure Unchecked_Deallocate_Storage
     (Pool : in out Dynamic_Pool) is abstract;
--   pragma Postcondition
--       (not Has_Allocations (Pool) and not Pool_Needs_Finalization (Pool));
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "For Ada 2012, use post'class");
   --  This call must have the effect of calling Unchecked_Deallocate_Objects
   --  for the specified Pool, and then releases all resources associated with
   --  the pools storage to the system, if possible. Whether or not new
   --  allocations can be made from the pool after this call is determined
   --  by the implementation of the derived type. If such allocations are
   --  allowed, then the allocations likely, though not necessarily, would
   --  involve new storage being to be allocated to the pool.

   procedure Unchecked_Deallocate_Storage
     (Subpool : in out Dynamic_Subpool_Handle) is abstract;
--   pragma Postcondition
--       (not Has_Allocations (Subpool) and
--        not Subpool_Needs_Finalization (Subpool));
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "For Ada 2012, use post'class");
   --  This call must have the effect of calling Unchecked_Deallocate_Objects
   --  for the specified Pool, and then releases all resources associated with
   --  the pools storage to the system, if possible. Whether or not new
   --  allocations can be made from the pool after this call is determined
   --  by the implementation of the derived type. If such allocations are
   --  allowed, then the allocations likely, though not necessarily, would
   --  involve new storage being to be allocated to the pool.

   function Has_Allocations
     (Pool : Dynamic_Pool) return Boolean is abstract;
   --  pragma Postcondition (not Objects_Need_Finalization (Pool));
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
      "For Ada 2012, use in out parameter instead of access");
   --  Returns true if there are currently objects allocated from the pool.
   --  This is class-wide because we don't want derivations to override this
   --  function possibly changing the effect of this call.

   function Has_Allocations
     (Subpool : Dynamic_Subpool_Handle) return Boolean is abstract;
   --  pragma Postcondition (not Objects_Need_Finalization (Subpool));
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
      "For Ada 2012, use in out parameter instead of access");
   --  Returns true if there are currently objects allocated from the subpool.
   --  This is class-wide because we don't want derivations to override this
   --  function possibly changing the effect of this call.

   function Pool_Needs_Finalization
     (Pool : Dynamic_Pool) return Boolean is abstract;
   --  Returns true if the Pool needs finalization, false otherwise.

   function Subpool_Needs_Finalization
     (Subpool : Dynamic_Subpool_Handle) return Boolean is abstract;
   --  Returns true if the Subpool needs finalization, false otherwise.

private

   type Dynamic_Pool is abstract new
     Sys.Storage_Pools.Subpools.Root_Storage_Pool_with_Subpools
   with null record;

   type Dynamic_Subpool is abstract new
     Sys.Storage_Pools.Subpools.Root_Subpool with null record;

   type Dynamic_Subpool_Handle is abstract new
     Sys.Storage_Pools.Subpools.Subpool_Handle with null record;

   pragma Inline
     (Unchecked_Deallocate_Objects,
      Has_Allocations);

end Dynamic_Pools;
