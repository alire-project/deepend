with System.Storage_Elements;

--  BJM needed to make this package look like a child of System.Storage_Pools
with System.Storage_Pools; use System; use System.Storage_Pools;

package Sys.Storage_Pools.Subpools is
   pragma Preelaborate (Sys.Storage_Pools.Subpools);

   type Root_Storage_Pool_with_Subpools is
      abstract new Root_Storage_Pool with private;
        --  An access type must have a storage pool of a type
        --  descended from this type to use subpools.

    type Root_Subpool is abstract tagged limited private;
        --  The subpool object type. Objects of this type are managed by
        --  the storage pool; usually only the handles are exposed
        --  to clients.

    type Subpool_Handle is access all Root_Subpool'Class;
    for Subpool_Handle'Storage_Size use 0;
        --  This provides a reference to a subpool, and serves to identify
        --  the subpool within the program.

--      function Create_Subpool(Pool : in out Root_Storage_Pool_with_Subpools;
--        Storage_Size : Storage_Elements.Storage_Count :=
--          Storage_Elements.Storage_Count'Last)
--            return not null Subpool_Handle is abstract;
   pragma Compile_Time_Warning
     (True, "For Ada 2012, use in out parameter instead of access");

   function Create_Subpool
     (Pool : access Root_Storage_Pool_with_Subpools;
      Storage_Size : Storage_Elements.Storage_Count :=
        Storage_Elements.Storage_Count'Last) return not null Subpool_Handle
      is abstract;
        --  Create subpool within given storage pool manager. Storage_Size
        --  specifies a limit on the amount of storage for the subpool.
        --  NOTE: Additional functions that create subpools may be
        --        defined for a given storage pool that supports subpools.
        --  [Editor's Note: This uses AI05-0143-1 to allow an in out
        --   parameter;
        --  "access" could be used instead but would be less convenient.]

   function Pool_of_Subpool
     (Subpool : not null Subpool_Handle)
        return access Root_Storage_Pool_with_Subpools'Class;
   --  Return access to underlying storage pool of given handle.

   procedure Set_Pool_of_Subpool
     (Subpool : not null Subpool_Handle;
      To : access Root_Storage_Pool_with_Subpools'Class);
   --  Set the Pool for a newly created subpool or a subpool which
   --  is being reused after a call to Deallocate_Subpool (this
   --  routine should only be used as part of the implementation of
   --  Create_Subpool or similar subpool constructors).
   --  Raises Program_Error if the Pool has already been set for
   --  Subpool since the last explicit finalization (if any) of the
   --  subpool.

--     BJM Uncommmenting the following crashes compiler
--     procedure Allocate_From_Subpool
--       (Pool : in out Root_Storage_Pool_with_Subpools;
--        Storage_Address : out Address;
--        Size_In_Storage_Elements : Storage_Elements.Storage_Count;
--        Alignment : Storage_Elements.Storage_Count;
--        Subpool : not null Subpool_Handle) is abstract;
   --  with Pre'Class => Pool_of_Subpool(Subpool) = Pool'access
        --  Allocate space from specified subpool.
        --  [Editor's note: The precondition is as described in AI05-0145-2
        --  and AI05-0183-1. It could be omitted if necessary.]
   procedure Deallocate_Subpool
     (Pool : in out Root_Storage_Pool_with_Subpools;
      Subpool : in out Subpool_Handle) is abstract;
   --  with Pre'Class => Pool_of_Subpool(Subpool) = Pool'access
        --  Deallocate the space for all of the objects allocated from the
        --  specified subpool, and destroy the subpool. The subpool handle
        --  is set to null after this call.

   function Default_Subpool_for_Pool
     (Pool : Root_Storage_Pool_with_Subpools)
      return not null Subpool_Handle;
   --  Returns a handle of the default subpool for Pool.
   --  Note: If no default subpool is supported, this routine should
   --  raise Storage_Error.

private
   type Root_Storage_Pool_with_Subpools is
      abstract new Root_Storage_Pool with null record;

   type Root_Subpool is abstract tagged limited record
      Pool : access Root_Storage_Pool_with_Subpools'Class;
   end record;

end Sys.Storage_Pools.Subpools;
