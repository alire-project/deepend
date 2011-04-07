package body Sys.Storage_Pools.Subpools is

   procedure Attach
     (Handle : in out Subpool_Handle;
      To : access Root_Subpool'Class) is
   begin
      Handle.Subpool := Subpool_Access (To);
      Handle.Subpool.Handle_Reference := Handle.Subpool'Unchecked_Access;
   end Attach;

   procedure Detach
     (Handle : in out Subpool_Handle) is
   begin
      Handle.Subpool.Handle_Reference := null;
      Handle.Subpool := null;
   end Detach;

   procedure Detach
     (Subpool : in out Root_Subpool'Class) is
   begin
      if Subpool.Handle_Reference /= null then
         Subpool.Handle_Reference.all := null;
         Subpool.Handle_Reference := null;
      end if;
   end Detach;

   function Get_Access (Subpool : Subpool_Handle'Class) return
     Subpool_Access is
   begin
      return Subpool.Subpool;
   end Get_Access;

--     function Get_Access_Reference (Subpool : Subpool_Handle'Class) return
--     access constant Subpool_Access is
--     begin
--        return Subpool.Subpool'Unchecked_Access;
--     end Get_Access_Reference;

   function Objects_Need_Finalization
     (Pool : Root_Storage_Pool_with_Subpools'Class) return Boolean is
      pragma Unreferenced (Pool);
   begin
      --  Ada Implementation needs to provide this, and currently this is
      --  not part of the Ada 2012 proposal, though maybe the proposal can
      --  be modified to include this...
      return False;
   end Objects_Need_Finalization;

   function Objects_Need_Finalization
     (Subpool : Subpool_Handle'Class) return Boolean is
      pragma Unreferenced (Subpool);
   begin
      --  Ada Implementation needs to provide this, and currently this is
      --  not part of the Ada 2012 proposal, though maybe the proposal can
      --  be modified to include this...
      return False;
   end Objects_Need_Finalization;

   function Pool_of_Subpool
     (Subpool : Subpool_Handle)
      return access Root_Storage_Pool_with_Subpools'Class is
   begin
      return Subpool.Subpool.Pool;
   end Pool_of_Subpool;

   procedure Set_Pool_of_Subpool
     (Subpool : in out Subpool_Handle;
      To : access Root_Storage_Pool_with_Subpools'Class) is
   begin
      Subpool.Subpool.Pool := To;
   end Set_Pool_of_Subpool;

end Sys.Storage_Pools.Subpools;
