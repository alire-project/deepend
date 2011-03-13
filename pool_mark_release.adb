------------------------------------------------------------------------------
--                                                                          --
--    Deepend - Mark and Release Storage Pool for Ada 2005 with Subpools    --
--                                                                          --
--                    P O O L _ M A R K _ R E L E A S E                     --
--                                                                          --
--                                B o d y                                   --
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

with Ada.Unchecked_Conversion;
with Interfaces;

package body Pool_Mark_Release is

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool         : in out Unbounded_Mark_Release_Pool;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Alignment);
   begin
      Address := Apache_Runtime.Pools.Allocate
        (Pool => Pool.Pool,
         Size => Apache_Runtime.Apr_Size (Storage_Size));
   end Allocate;

   --------------------------------------------------------------

   function Allocation
     (Pool : access Unbounded_Mark_Release_Pool)
      return Allocation_Type_Access
   is
      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => Allocation_Type_Access);
   begin
      pragma Assert (not Is_Finalized (Pool.all) and
                       Is_Owner (Pool.all, Current_Task));
      return Convert
        (Apache_Runtime.Pools.Allocate
           (Pool => Pool.all.Pool,
            Size => Apache_Runtime.Apr_Size
              (Allocation_Type'Size / Interfaces.Unsigned_8'Size)));
   end Allocation;

   --------------------------------------------------------------

--     procedure Allocation_Proc
--       (Pool : in out Unbounded_Mark_Release_Pool;
--        New_Item : out Allocation_Type_Access)
--     is
--        function Convert is new Ada.Unchecked_Conversion
--          (Source => System.Address,
--           Target => Allocation_Type_Access);
--     begin
--        pragma Assert (not Is_Finalized (Pool.all) and
--                         Is_Owner (Pool.all, Current_Task));
--        New_Item := Convert
--          (Apache_Runtime.Pools.Allocate
--             (Pool => Pool.all.Pool,
--              Size => Apache_Runtime.Apr_Size
--                (Allocation_Type'Size / Interfaces.Unsigned_8'Size)));
--     end Allocation_Proc;

   --------------------------------------------------------------

   function Create_Subpool
     (Parent : access Unbounded_Mark_Release_Pool)
      return Unbounded_Mark_Release_Pool
   is
      use type Apache_Runtime.Apr_Status;
   begin
      return  New_Pool : Unbounded_Mark_Release_Pool
        := (System.Storage_Pools.Root_Storage_Pool with
            Mode => Auto_Unchecked_Deallocation,
            Declaring_Task_Allocates => True,
            Pool => System.Null_Address,
            Owner => Ada.Task_Identification.Current_Task,
            Is_A_Subpool => True)
      do

         if Apache_Runtime.Pools.Create
          (New_Pool => New_Pool.Pool'Address,
           Parent   => Parent.all.Pool) /= 0 then
            raise Storage_Error;
         end if;
         New_Pool.Is_A_Subpool := True;
      end return;

   end Create_Subpool;

   --------------------------------------------------------------

   overriding procedure Finalize
     (Item : in out Unbounded_Mark_Release_Pool) is
   begin
      --  Subpools dont get finalized until the top level parent is finalized
      if not Item.Is_A_Subpool then
         Apache_Runtime.Pools.Destroy (Item.Pool);
         Item.Pool := System.Null_Address;
      end if;
   end Finalize;

   --------------------------------------------------------------

--     function Get_Parent
--       (Child : Unbounded_Mark_Release_Pool)
--        return Unbounded_Mark_Release_Pool
--     is
--     begin
--        --  We cant say for sure that the parent is a subpool, but it
--        --  doesn't really matter. This really is providing a copy of the
--        --  real pool object. We dont want this object to be associated with
--        --  any finalization, so setting Is_A_Subpool to true is OK here.
--        return  Pool : Unbounded_Mark_Release_Pool
--          := (System.Storage_Pools.Root_Storage_Pool with
--              Mode => Auto_Unchecked_Deallocation,
--              Pool => System.Null_Address,
--              Owner =>
--              Is_A_Subpool => True)
--        do
--           Pool.Pool :=
--             Apache_Runtime.Pools.Get_Parent (Child => Child.Pool);
--        end return;
--     end Get_Parent;

   --------------------------------------------------------------

   overriding procedure Initialize
     (Item : in out Unbounded_Mark_Release_Pool)
   is
      use type Apache_Runtime.Apr_Status;
   begin
      if Apache_Runtime.Pools.Create
          (New_Pool => Item.Pool'Address,
           Parent   => System.Null_Address) /= 0 then
         raise Storage_Error;
      end if;

      if Item.Declaring_Task_Allocates then
         Item.Owner := Ada.Task_Identification.Current_Task;
      else
         Item.Owner := Ada.Task_Identification.Null_Task_Id;
      end if;

      Item.Is_A_Subpool := False;
   end Initialize;

   --------------------------------------------------------------

   function Is_Ancestor
     (Ancestor, Child : Unbounded_Mark_Release_Pool) return Boolean is
   begin
      return Apache_Runtime.Pools.Is_Ancestor (Ancestor.Pool, Child.Pool);
   end Is_Ancestor;

   --------------------------------------------------------------

   function Is_Finalized
     (Pool : Unbounded_Mark_Release_Pool) return Boolean is
   begin
      return (Pool.Pool = System.Null_Address);
   end Is_Finalized;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Unbounded_Mark_Release_Pool;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   procedure Release
     (Pool : in out Unbounded_Mark_Release_Pool) is
   begin
      Apache_Runtime.Pools.Clear (Pool.Pool);
   end Release;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Unbounded_Mark_Release_Pool;
      T : Task_Id := Current_Task) is
   begin
      Pool.Owner := T;
   end Set_Owner;

   ------------------
   -- Storage_Size --
   ------------------

   overriding function Storage_Size
     (Pool : Unbounded_Mark_Release_Pool)
      return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return System.Storage_Elements.Storage_Count'Last;
   end Storage_Size;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate
     (Pool : in out Unbounded_Mark_Release_Pool) is
   begin
      Apache_Runtime.Pools.Destroy (Pool.Pool);
      Pool.Pool := System.Null_Address;
   end Unchecked_Deallocate;

   Initialize_Status : constant Apache_Runtime.Apr_Status :=
     Apache_Runtime.Pools.Initialize;

   use type Apache_Runtime.Apr_Status;
begin
   if Initialize_Status /= 0 then
      raise Program_Error;
   end if;
end Pool_Mark_Release;
