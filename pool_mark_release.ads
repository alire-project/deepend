------------------------------------------------------------------------------
--                                                                          --
--    Deepend - Mark and Release Storage Pool for Ada 2005 with Subpools    --
--                                                                          --
--                    P O O L _ M A R K _ R E L E A S E                     --
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

--  A Mark and Release Storage pool that provides a binding to the
--  Apache Runtime Pools Implementation. In addition to Mark and
--  Release memory management, the Storage pool also provides
--  Subpool capabilities.

with System.Storage_Pools;
with System.Storage_Elements;
with Ada.Task_Identification; use Ada.Task_Identification;

private with Apache_Runtime.Pools;

package Pool_Mark_Release is
   pragma Elaborate_Body;
   --  Needed to ensure that library routines can execute allocators

   --  Allocation strategy:

   --    Pools can be automatically finalized if declared with
   --      Auto_Unchecked_Deallocation mode.

   --    Pools must be explicitly finalized with Unchecked_Deallocation if
   --      declared with Manual_Unchecked_Deallocation mode.

   --    Deallocate is not needed or used, and is implemented as a null
   --    procedure. Use of this storage pool means that there is no need for
   --    calls to Ada.Unchecked_Deallocation.
   --
   --    no user specifiable size
   --    no automatic reclaim
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

   use type System.Address;

   type Mode_Kinds is
     (Auto_Unchecked_Deallocation,
      Manual_Unchecked_Deallocation);
   --  Auto_Unchecked_Deallocation Mode permits the root storage pool to
   --  finalize without an explicit call to Unchecked_Pool_Deallocation.
   --
   --  Manual_Unchecked_Deallocation Mode requires that an explicit call to
   --  Unchecked_Pool_Deallocation be made before the storage pool is
   --  finalized.

   type Unbounded_Mark_Release_Pool
     (Mode : Mode_Kinds;
      Declaring_Task_Allocates : Boolean) is new
     System.Storage_Pools.Root_Storage_Pool with private;

   overriding function Storage_Size
     (Pool : Unbounded_Mark_Release_Pool)
      return System.Storage_Elements.Storage_Count;

   pragma Precondition (not Is_Finalized (Pool));

   overriding procedure Allocate
     (Pool         : in out Unbounded_Mark_Release_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count);

   pragma Precondition (not Is_Finalized (Pool) and
                          Is_Owner (Pool, Current_Task));
   --  Allocate must be called by the task that created the Pool or Subpool.
   --  The pool may be a subpool of a pool owned by a different task
   --  however.

   overriding procedure Deallocate
     (Pool         : in out Unbounded_Mark_Release_Pool;
      Address      : System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count)
   is null;
   --  Deallocate is not meant to be called, so it has no effect.
   --  This is a mark-release pool, Deallocation occurs when the
   --  Storage pool object is finalized (or when Release is called).
   --  The nice thing about this, there is no need to use
   --  Unchecked_Deallocation.

   procedure Release
     (Pool : in out Unbounded_Mark_Release_Pool);
   --  Releases all memory in the pools. This does not actually free the
   --  memory, it just allows the memory to be reused for subsequent
   --  allocations.

   pragma Precondition (not Is_Finalized (Pool) and
                          Is_Owner (Pool, Current_Task));

   function Create_Subpool
     (Parent : access Unbounded_Mark_Release_Pool)
      return Unbounded_Mark_Release_Pool;
   --  The lifetime of a subpool is the same as that of its parent pool.
   --
   --  This function is thread-safe, in the sense that multiple tasks
   --  can safely create subpools of the same parent pool concurrently.
   --  Similarly, a subpool can be created by one task at the same
   --  time that another thread accesses the parent pool.

   pragma Compile_Time_Warning
     (True,
      "For Ada 2012, use in out parameter instead of access");

   pragma Precondition (not Is_Finalized (Parent.all));
   pragma Postcondition (not Is_Finalized (Create_Subpool'Result) and
                        Is_Ancestor (Parent.all, Create_Subpool'Result));

   procedure Unchecked_Deallocate
     (Pool : in out Unbounded_Mark_Release_Pool);

   pragma Precondition (not Is_Finalized (Pool) and
                          Is_Owner (Pool, Current_Task));
   pragma Postcondition (Is_Finalized (Pool));

   function Is_Ancestor
     (Ancestor, Child : Unbounded_Mark_Release_Pool) return Boolean;
   --  Returns True is Ancestor is an ancestor of Child

   pragma Precondition (not Is_Finalized (Ancestor) and
                        not Is_Finalized (Child));

   function Is_Finalized
     (Pool : Unbounded_Mark_Release_Pool) return Boolean;

   function Is_Owner
     (Pool : Unbounded_Mark_Release_Pool;
      T : Task_Id := Current_Task) return Boolean;

   pragma Precondition (not Is_Finalized (Pool));

   procedure Set_Owner
     (Pool : in out Unbounded_Mark_Release_Pool;
      T : Task_Id := Current_Task);

   pragma Precondition (Is_Owner (Pool, Null_Task_Id));
   pragma Postcondition (Is_Owner (Pool, Current_Task));

   generic
      type Allocation_Type is private;
      type Allocation_Type_Access is access Allocation_Type;
   function Allocation
     (Pool : access Unbounded_Mark_Release_Pool) return Allocation_Type_Access;

   pragma Compile_Time_Warning
     (True,
      "For Ada 2012, use in out parameter instead of access");
   --  This generic routine provides a mechanism to allocate from a subpool.
   --  The "new" has to be associated with the root storage pool, and currently
   --  there is no way to override the storage pool object for the "new"
   --  operator.
   --
   --  This function allows the storage pool object to be specified, which
   --  may be either the root pool object, or any subpool object.

--     generic
--        type Allocation_Type is private;
--        type Allocation_Type_Access is access Allocation_Type;
--     procedure Allocation_Proc
--       (Pool : in out Unbounded_Mark_Release_Pool;
--        New_Item : out Allocation_Type_Access);

private

   type Unbounded_Mark_Release_Pool
     (Mode : Mode_Kinds;
      Declaring_Task_Allocates : Boolean) is
     new System.Storage_Pools.Root_Storage_Pool with
      record
         Pool : Apache_Runtime.Pools.Pool_Type;
         Is_A_Subpool : Boolean;
         Owner : Ada.Task_Identification.Task_Id;
      end record;

   overriding procedure Initialize (Item : in out Unbounded_Mark_Release_Pool);

   pragma Postcondition (not Item.Is_A_Subpool);

   overriding procedure Finalize   (Item : in out Unbounded_Mark_Release_Pool);

   pragma Precondition
     (Item.Mode = Auto_Unchecked_Deallocation or
        Item.Pool = System.Null_Address or Item.Is_A_Subpool);

   pragma Inline (Storage_Size, Allocate, Create_Subpool, Release);
   pragma Inline (Is_Ancestor, Is_Finalized, Is_Owner, Set_Owner);
--     pragma Inline (Get_Parent);

end Pool_Mark_Release;
