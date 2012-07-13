--  This is intended to closely map the proposal for Ada 2012 for Storage
--  Subpools, and represents the package System.Storage_Pools.Subpools as
--  defined in AI05-0111-3. When Ada 2012 becomes available, this package will
--  be deleted from Deepend.

package body Sys.Storage_Pools.Subpools is

   overriding
   procedure Allocate
     (Pool : in out Root_Storage_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      null;
   end Allocate;

   function Pool_Of_Subpool
     (Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_With_Subpools'Class is
   begin
      return Subpool.Pool;
   end Pool_Of_Subpool;

   procedure Set_Pool_Of_Subpool
     (Subpool : not null Subpool_Handle;
      To : in out Root_Storage_Pool_With_Subpools'Class) is
   begin
      Subpool.Pool := To'Unchecked_Access;
   end Set_Pool_Of_Subpool;

   overriding
   function Storage_Size
     (Pool : Root_Storage_Pool_With_Subpools)
      return Storage_Count is
      pragma Unreferenced (Pool);
   begin
      return Storage_Count'Last;
   end Storage_Size;

   function Default_Subpool_For_Pool
     (Pool : Root_Storage_Pool_With_Subpools)
      return not null Subpool_Handle is
   begin
      raise Program_Error;
      return Default_Subpool_For_Pool (Pool);
   end Default_Subpool_For_Pool;

end Sys.Storage_Pools.Subpools;
