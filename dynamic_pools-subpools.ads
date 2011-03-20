------------------------------------------------------------------------------
--                                                                          --
--                   Deepend - Dynamic Pools for Ada 2005                   --
--                                                                          --
--                D Y N A M I C   P O O L S . S U B P O O L S               --
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

--  A Subpool is a type of dynamic storage pool for Ada 2005 where all the
--  objects in a subpool can be reclaimed all at once, instead of requiring
--  each object to be individually reclaimed one at a time. Subpools may be
--  completely independent pool objects, or they may be chained together in
--  order to extend the lifetime of child pools to that of the lifetime of
--  the root parent pool. In addition, the pool may be reclaimed multiple
--  times before the end of its lifetime through an explicit call to
--  Unchecked_Deallocate_Objects or Unchecked_Deallocate_Storage.
--
--  A Subpool currently provides a binding to the Apache Runtime Pools
--  Implementation. This may change in the future, as it is desirable to
--  provide a solution written entirely in Ada.

with System.Storage_Elements;
with Ada.Task_Identification; use Ada.Task_Identification;

private with Apache_Runtime.Pools;

package Dynamic_Pools.Subpools is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   --  Allocation strategy:

   --    Pools can be automatically finalized if declared with
   --      Auto_Unchecked_Deallocation mode.

   --    Pools must be explicitly finalized with Unchecked_Deallocation if
   --      declared with Manual_Unchecked_Deallocation mode.

   --    no user specifiable size
   --    minimal overhead

   --  ** NOTE: It is erroneous to allocate objects that need finalization
   --  eg. (Tasks, or objects of types inherited from types defined in
   --       Ada.Finalization)
   --  and then Release the storage associated with those objects before
   --  they would have otherwise been finalized.

   --  Note that most operations on pools are not thread-safe: a single pool
   --  should only be accessed by a single task at any given time. The one
   --  exception to this rule is creating a subpool of a given pool: one or
   --  more tasks can safely create subpools at the same time that another
   --  task accesses the parent pool.

   type Mode_Kinds is
     (Auto_Unchecked_Deallocation,
      Manual_Unchecked_Deallocation);
   --  Auto_Unchecked_Deallocation Mode permits the root storage pool to
   --  finalize without an explicit call to Unchecked_Deallocate_Storage or
   --  Unchecked_Deallocate_Objects.
   --
   --  Manual_Unchecked_Deallocation Mode requires that an explicit call to
   --  Unchecked_Deallocate_Storage or Unchecked_Deallocate_Objects be made
   --  before the storage pool is finalized. This is enforced by
   --  preconditions on the Finalize procedure. There are some that might
   --  feel that Unchecked_Deallocation, due to its unsafe nature, should
   --  be explicitly acknowledged in the code.

   type Dynamic_Pool_With_Subpools
     (Mode : Mode_Kinds;
      Declaring_Task_Is_Owner : Boolean) is new Dynamic_Pool with private;
   pragma Compile_Time_Warning
     (Ada2012_Warnings, "In Ada 2012, use discriminant defaults");
   --  Default for Mode => Auto_Unchecked_Deallocation
   --  Default for Declaring_Task_Is_Owner => True

   --  Only one task can allocate from a particular pool object. Once the
   --  owning task has been specified, it is assigned for the remainder of the
   --  lifetime of the pool. It cannot be reassigned.
   --  Declaring_Task_Is_Owner specifies that the task declaring the pool is
   --  the owner. Otherwise, the owner is not yet specified, and will be
   --  specified later, through the Set_Owner call, before any allocations can
   --  be made from the pool.

   overriding function Storage_Size
     (Pool : Dynamic_Pool_With_Subpools)
      return System.Storage_Elements.Storage_Count;

   overriding procedure Allocate
     (Pool         : in out Dynamic_Pool_With_Subpools;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);
   pragma Precondition (Is_Owner (Pool, Current_Task) and
                        Pool_Needs_Finalization (Pool));
   --  Allocate must be called by the task that created the Pool or Subpool.
   --  The pool may be a subpool of a pool owned by a different task
   --  however. We don't allow further allocation from the pool once its
   --  storage has been deallocated.

   overriding procedure Deallocate
     (Pool         : in out Dynamic_Pool_With_Subpools;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count)
   is null;
   pragma Precondition (Is_Owner (Pool, Current_Task) and
                        Pool_Needs_Finalization (Pool));
   --  Even though the parent class already defines Deallocate as a null
   --  procedure, it seems worthwhile to override it here to provide the
   --  preconditions, just to be sure client usage is correct.

   function Create_Subpool
     (Parent : access Dynamic_Pool_With_Subpools)
      return Dynamic_Pool_With_Subpools;
   pragma Precondition (Pool_Needs_Finalization (Parent.all));
   pragma Postcondition (Pool_Needs_Finalization (Create_Subpool'Result) and
                         Is_Ancestor (Parent.all, Create_Subpool'Result));
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
      "For Ada 2012, use in out parameter instead of access");
   --  The default lifetime of a subpool is the same as that of its
   --  ultimate ancestor pool. The ultimate ancestor pool is the top level
   --  ancestor of the pool that does not itself have a parent pool.
   --
   --  This function is thread-safe, in the sense that multiple tasks
   --  can safely create subpools of the same parent pool concurrently.
   --  Similarly, a subpool can be created by one task at the same
   --  time that another task accesses the parent pool.

   overriding procedure Unchecked_Deallocate_Objects
     (Pool : in out Dynamic_Pool_With_Subpools);
   pragma Precondition (Pool_Needs_Finalization (Pool));
   --  This call first has the effect of calling Unchecked_Deallocate_Storage
   --  for all subpools of the pool. It then has the effect of calling
   --  Dynamic_Pools.Unchecked_Deallocate_Objects (the parent type
   --  implementation) to performs unchecked storage deallocation of all
   --  objects allocated from the pool. After this call, the Pool will still
   --  need finalization of its storage. The intent is to allow new objects to
   --  be allocated from the Pool after this call, but eliminating the overhead
   --  of deallocating the pools storage and then having to allocate new
   --  storage for the pool for subsequent allocations. i.e., This allows
   --  existing storage to be reused. If the pool is a subpool, then it remains
   --  a subpool to its parent after this call, though all subpools of the pool
   --  will have been orphaned and disabled from further allocations.

   procedure Unchecked_Deallocate_Storage
     (Pool : in out Dynamic_Pool_With_Subpools);
   pragma Precondition (Pool_Needs_Finalization (Pool));
   pragma Postcondition
     (not Objects_Need_Finalization (Pool) and
      not Pool_Needs_Finalization (Pool));
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
      "For Ada 2012, Post condition is 'class of parent class");
   --  This call must have the effect of making a dispatching call to
   --  Unchecked_Deallocate_Objects for the specified Pool, and then releases
   --  all resources associated with the pools storage to the system.
   --  Further allocations from the pool are disallowed. If the pool is a
   --  subpool, it will be orphaned and will no longer be a subpool of another
   --  pool after this call.

   function Is_Ancestor
     (Ancestor, Child : Dynamic_Pool_With_Subpools) return Boolean;
   --  Returns True is Ancestor is an ancestor of Child, i.e., the Child
   --  is a subpool of the Ancestor, or its parent is a subpool or the
   --  ancestor, or its parents parent is a subpool, and so on recursively
   --  to the ultimate ancestor.

   function Is_Owner
     (Pool : Dynamic_Pool_With_Subpools;
      T : Task_Id := Current_Task) return Boolean;
   pragma Precondition (Pool_Needs_Finalization (Pool));
   --  Returns True if the specified task owns the pool/subpool and thus is
   --  allowed to allocate from it.

   procedure Set_Owner
     (Pool : in out Dynamic_Pool_With_Subpools;
      T : Task_Id := Current_Task);
   pragma Precondition (Is_Owner (Pool, Null_Task_Id));
   pragma Postcondition (Is_Owner (Pool, T));
   --  The task that owns a pool/subpool and therefore allowed to allocate
   --  from it, can only be specified once for a particular pool.

   function Is_A_Subpool
     (Pool : Dynamic_Pool_With_Subpools'Class) return Boolean;
   --  Returns True if Pool is a subpool of another pool. Returns false
   --  otherwise.

   function Pool_Needs_Finalization
     (Pool : Dynamic_Pool_With_Subpools) return Boolean;
      --  Returns True if Unchecked_Deallocate_Storage has not been called for
      --  the pool, and if the pool is a subpool then neither
      --  Unchecked_Deallocate_Objects or Unchecked_Deallocate_Storage has
      --  been called for any of the pools ancestors. Returns false otherwise.

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Allocation
     (Pool : access Dynamic_Pool_With_Subpools) return Allocation_Type_Access;
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
      "For Ada 2012, use in out parameter instead of access");
   --  This generic routine provides a mechanism to allocate an object of
   --  a definite subtype from a pool.
   --  The "new" has to be associated with the root storage pool, and currently
   --  there is no way to override the storage pool object for the "new"
   --  operator.
   --
   --  This function allows the storage pool object to be specified for an
   --  allocation.

   generic
      type Allocation_Type (<>) is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Initialized_Allocation
     (Pool : access Dynamic_Pool_With_Subpools;
      Qualified_Expression : Allocation_Type) return Allocation_Type_Access;
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
      "For Ada 2012, use in out parameter instead of access");
   --  This generic routine provides a mechanism to allow an object of an
   --  indefinite subtype, or a qualified expression from a pool.
   --  The "new" has to be associated with the root storage pool, and currently
   --  there is no way to override the storage pool object for the "new"
   --  operator.
   --
   --  This function allows the storage pool object to be specified for an
   --  allocation.

private

   type Dynamic_Pool_With_Subpools
     (Mode : Mode_Kinds;
      Declaring_Task_Is_Owner : Boolean) is
     new Dynamic_Pool with
      record
         Pool : Apache_Runtime.Pools.Pool_Type;
         Is_Subpool : Boolean;
         Owner : Ada.Task_Identification.Task_Id;
      end record;

   overriding procedure Initialize (Item : in out Dynamic_Pool_With_Subpools);
   pragma Postcondition (not Item.Is_A_Subpool);

   overriding procedure Finalize   (Item : in out Dynamic_Pool_With_Subpools);
   pragma Precondition
     (Item.Mode = Auto_Unchecked_Deallocation or
        Item.Pool = System.Null_Address or Item.Is_A_Subpool);

   pragma Inline (Storage_Size, Allocate, Create_Subpool);
   pragma Inline (Is_Ancestor, Is_A_Subpool, Is_Owner, Set_Owner);
   pragma Inline (Unchecked_Deallocate_Objects);
   pragma Inline (Unchecked_Deallocate_Storage);

end Dynamic_Pools.Subpools;
