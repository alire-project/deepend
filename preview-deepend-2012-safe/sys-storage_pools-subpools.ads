with Ada.Finalization;
with System.Storage_Elements;

--  BJM needed to make this package look like a child of System.Storage_Pools
with System.Storage_Pools; use System; use System.Storage_Pools;

private with Ada.Unchecked_Deallocation;

package Sys.Storage_Pools.Subpools is
   pragma Preelaborate (Sys.Storage_Pools.Subpools);

   Ada2012_Warnings : constant Boolean := False;

   type Root_Storage_Pool_with_Subpools is
      abstract new Root_Storage_Pool with private;
        --  An access type must have a storage pool of a type
        --  descended from this type to use subpools.

    type Root_Subpool is abstract tagged limited private;
        --  The subpool object type. Objects of this type are managed by
        --  the storage pool; usually only the handles are exposed
        --  to clients.

   type Subpool_Handle is abstract new
     Ada.Finalization.Limited_Controlled with private;
--      type Subpool_Handle is access all Root_Subpool'Class;
--      for Subpool_Handle'Storage_Size use 0;
        --  This provides a reference to a subpool, and serves to identify
        --  the subpool within the program.

--      function Create_Subpool(Pool : in out Root_Storage_Pool_with_Subpools;
--        Storage_Size : Storage_Elements.Storage_Count :=
--          Storage_Elements.Storage_Count'Last)
--            return not null Subpool_Handle is abstract;
--   pragma Compile_Time_Warning
--     (True, "For Ada 2012, use in out parameter instead of access");

   function Create_Subpool
     (Pool : access Root_Storage_Pool_with_Subpools;
      Storage_Size : Storage_Elements.Storage_Count :=
        Storage_Elements.Storage_Count'Last) return Subpool_Handle'Class
      is abstract;
        --  Create subpool within given storage pool manager. Storage_Size
        --  specifies a limit on the amount of storage for the subpool.
        --  NOTE: Additional functions that create subpools may be
        --        defined for a given storage pool that supports subpools.
        --  [Editor's Note: This uses AI05-0143-1 to allow an in out
        --   parameter;
        --  "access" could be used instead but would be less convenient.]

   function Objects_Need_Finalization
     (Pool : Root_Storage_Pool_with_Subpools'Class) return Boolean;
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
      "For Ada 2012, use in out parameter instead of access");
   --  Returns true if there are objects allocated from the pool that have
   --  not been deallocated and need finalization. This is class-wide because
   --  we don't want derivations to override this function possibly changing
   --  the effect of this call.

   function Objects_Need_Finalization
     (Subpool : Subpool_Handle'Class) return Boolean;
   pragma Compile_Time_Warning
     (Ada2012_Warnings,
      "For Ada 2012, use in out parameter instead of access");
   --  Returns true if there are objects allocated from the subpool that have
   --  not been deallocated and need finalization. This is class-wide because
   --  we don't want derivations to override this function possibly changing
   --  the effect of this call.

   function Pool_of_Subpool
     (Subpool : Subpool_Handle)
        return access Root_Storage_Pool_with_Subpools'Class;
   --  Return access to underlying storage pool of given handle.

   procedure Set_Pool_of_Subpool
     (Subpool : in out Subpool_Handle;
      To : access Root_Storage_Pool_with_Subpools'Class);
   --  Set the Pool for a newly created subpool or a subpool which
   --  is being reused after a call to Deallocate_Subpool (this
   --  routine should only be used as part of the implementation of
   --  Create_Subpool or similar subpool constructors).
   --  Raises Program_Error if the Pool has already been set for
   --  Subpool since the last explicit finalization (if any) of the
   --  subpool.

   procedure Allocate_From_Subpool
     (Pool : in out Root_Storage_Pool_with_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : in out Subpool_Handle'Class) is abstract;
   --  with Pre'Class => Pool_of_Subpool(Subpool) = Pool'access
        --  Allocate space from specified subpool.
        --  [Editor's note: The precondition is as described in AI05-0145-2
        --  and AI05-0183-1. It could be omitted if necessary.]

   procedure Deallocate_Subpool
     (Pool : in out Root_Storage_Pool_with_Subpools;
      Subpool : in out Subpool_Handle'Class) is abstract;
   --  with Pre'Class => Pool_of_Subpool(Subpool) = Pool'access
        --  Deallocate the space for all of the objects allocated from the
        --  specified subpool, and destroy the subpool. The subpool handle
        --  is set to null after this call.

   function Default_Subpool_for_Pool
     (Pool : Root_Storage_Pool_with_Subpools)
      return Subpool_Handle'Class is abstract;
   --  Returns a handle of the default subpool for Pool.
   --  Note: If no default subpool is supported, this routine should
   --  raise Storage_Error.

   procedure Attach
     (Handle : in out Subpool_Handle;
      To : access Root_Subpool'Class);
--   pragma Precondition (Pool_of_Subpool (Handle) = null);
   --  Attach the Subpool_Handle to  for a newly created subpool or a subpool
   --  which is being reused after a call to Deallocate_Subpool (this
   --  routine should only be used as part of the implementation of
   --  Create_Subpool or similar subpool constructors).
   --  Raises Program_Error if the handle is already attached to a subpool
   --  since the last explicit finalization (if any) of the subpool.

   procedure Detach
     (Handle : in out Subpool_Handle);
   pragma Precondition (Pool_of_Subpool (Handle) /= null);
   --  Detach the Subpool_Handle from its subpool, as part of the
   --  finalization of the subpool handle. The subpool may live longer than
   --  than the subpool handle. Once the handle has been detached from
   --  the subpool, deallocating the subpool will not result in attempting to
   --  clean up the dangling reference from the subpool handle, since it is
   --  assumed the subpool handle no longer exists.
   --  Raises Program_Error if the handle is already detached from its subpool.

   procedure Detach
     (Subpool : in out Root_Subpool'Class);
   --  pragma Precondition (Pool_of_Subpool (Handle) /= null);
   --  Detach the Subpool_Handle from its subpool, as part of the
   --  finalization of the subpool handle. The subpool may live longer than
   --  than the subpool handle. Once the handle has been detached from
   --  the subpool, deallocating the subpool will not result in attempting to
   --  clean up the dangling reference from the subpool handle, since it is
   --  assumed the subpool handle no longer exists.
   --  Raises Program_Error if the handle is already detached from its subpool.

   type Subpool_Access is access all Root_Subpool'Class;

   function Get_Access (Subpool : Subpool_Handle'Class) return
     Subpool_Access;

--     function Get_Access_Reference (Subpool : Subpool_Handle'Class) return
--       access constant Subpool_Access;

private

   type Root_Storage_Pool_with_Subpools is
      abstract new Root_Storage_Pool with null record;

   type Root_Subpool is abstract tagged limited
      record
         Pool : access Root_Storage_Pool_with_Subpools'Class;
         Handle_Reference : access Subpool_Access;
         --  While the Subpool_Handle exists, this component provides
         --  a means to set the subpool handle's reference to null if the
         --  subpool is deallocated.
         --  Conversely, if the Subpool_Handle ceases to exist before the
         --  subpool is deallocated, its finalization needs to set the
         --  Handle_Reference to null, so that when the subpool is deallocated,
         --  it knows not to set the handles reference to null, since the
         --  handle no longer exists.
   end record;

   type Subpool_Handle is abstract new
     Ada.Finalization.Limited_Controlled with
      record
         Subpool : aliased Subpool_Access;
      end record;

   procedure Free_Subpool is new Ada.Unchecked_Deallocation
     (Object => Root_Subpool'Class,
      Name => Subpool_Access);

end Sys.Storage_Pools.Subpools;
