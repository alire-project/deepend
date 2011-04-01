package body Sys.Storage_Pools.Subpools is

   function Pool_of_Subpool
     (Subpool : not null Subpool_Handle)
      return access Root_Storage_Pool_with_Subpools'Class is
   begin
      return Subpool.Pool;
   end Pool_of_Subpool;

   procedure Set_Pool_of_Subpool
     (Subpool : not null Subpool_Handle;
      To : access Root_Storage_Pool_with_Subpools'Class) is
   begin
      Subpool.Pool := To;
   end Set_Pool_of_Subpool;

   function Default_Subpool_for_Pool
     (Pool : Root_Storage_Pool_with_Subpools)
      return not null Subpool_Handle
   is
      Result : Subpool_Handle;
   begin
      return Result;
   end Default_Subpool_for_Pool;

end Sys.Storage_Pools.Subpools;
