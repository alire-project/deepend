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

package body Dynamic_Pools.Subpools is

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool         : in out Dynamic_Pool_With_Subpools;
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

   function Create_Subpool
     (Parent : access Dynamic_Pool_With_Subpools)
      return Dynamic_Pool_With_Subpools
   is
      use type Apache_Runtime.Apr_Status;
   begin
      return  New_Pool : Dynamic_Pool_With_Subpools
        := (Dynamic_Pool with
            Mode => Auto_Unchecked_Deallocation,
            Declaring_Task_Is_Owner => True,
            Pool => System.Null_Address,
            Owner => Ada.Task_Identification.Current_Task,
            Is_Subpool => True)
      do
         if Apache_Runtime.Pools.Create
          (New_Pool => New_Pool.Pool'Address,
           Parent   => Parent.all.Pool) /= 0 then
            raise Storage_Error;
         end if;
      end return;

   end Create_Subpool;

   --------------------------------------------------------------

   overriding procedure Finalize
     (Item : in out Dynamic_Pool_With_Subpools) is
   begin
      --  Subpools dont get finalized until the top level parent is finalized
      if not Item.Is_Subpool then
         Apache_Runtime.Pools.Destroy (Item.Pool);
         Item.Pool := System.Null_Address;
      end if;
   end Finalize;

   --------------------------------------------------------------

   overriding procedure Initialize
     (Item : in out Dynamic_Pool_With_Subpools)
   is
      use type Apache_Runtime.Apr_Status;
   begin
      if Apache_Runtime.Pools.Create
          (New_Pool => Item.Pool'Address,
           Parent   => System.Null_Address) /= 0 then
         raise Storage_Error;
      end if;

      if Item.Declaring_Task_Is_Owner then
         Item.Owner := Ada.Task_Identification.Current_Task;
      else
         Item.Owner := Ada.Task_Identification.Null_Task_Id;
      end if;

      Item.Is_Subpool := False;
   end Initialize;

   --------------------------------------------------------------

   function Is_A_Subpool
     (Pool : Dynamic_Pool_With_Subpools'Class) return Boolean is
   begin
      return Pool.Is_Subpool;
   end Is_A_Subpool;

   --------------------------------------------------------------

   function Is_Ancestor
     (Ancestor, Child : Dynamic_Pool_With_Subpools) return Boolean is
   begin
      return Apache_Runtime.Pools.Is_Ancestor (Ancestor.Pool, Child.Pool);
   end Is_Ancestor;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Dynamic_Pool_With_Subpools;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   function Pool_Needs_Finalization
     (Pool : Dynamic_Pool_With_Subpools) return Boolean is
   begin
      return (Pool.Pool /= System.Null_Address);
   end Pool_Needs_Finalization;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Dynamic_Pool_With_Subpools;
      T : Task_Id := Current_Task) is
   begin
      Pool.Owner := T;
   end Set_Owner;

   ------------------
   -- Storage_Size --
   ------------------

   overriding function Storage_Size
     (Pool : Dynamic_Pool_With_Subpools)
      return System.Storage_Elements.Storage_Count
   is
      pragma Unreferenced (Pool);
   begin
      return System.Storage_Elements.Storage_Count'Last;
   end Storage_Size;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Objects
     (Pool : in out Dynamic_Pool_With_Subpools) is
   begin
      --  If we had access to the Ada finalization functionality, we would
      --  have stored that as user data in the Apache Pool,
      --  by calling apr_pool_userdata_set, and would have
      --  specified the routines to be called when the pools are cleared or
      --  destroyed in in apr_pool_cleanup_register.
      --  Since we don't have access to this functionality in Ada 2005, all we
      --  can do is call Clear.

      Apache_Runtime.Pools.Clear (Pool.Pool);
   end Unchecked_Deallocate_Objects;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Storage
     (Pool : in out Dynamic_Pool_With_Subpools) is
   begin
      Apache_Runtime.Pools.Destroy (Pool.Pool);
      Pool.Pool := System.Null_Address;
   end Unchecked_Deallocate_Storage;

   Initialize_Status : constant Apache_Runtime.Apr_Status :=
     Apache_Runtime.Pools.Initialize;

   use type Apache_Runtime.Apr_Status;

   function Allocation
     (Pool : access Dynamic_Pool_With_Subpools)
      return Allocation_Type_Access
   is
      Location : System.Address;

      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => Allocation_Type_Access);
      use type System.Storage_Elements.Storage_Offset;
   begin

      pragma Assert (Allocation_Type_Access'Storage_Size /= 0);
      pragma Compile_Time_Warning
        (Ada2012_Warnings,
         "In Ada 2012, this should be a precondition");

      Allocate
        (Pool => Pool.all,
         Address => Location,
         Storage_Size =>
           Allocation_Type'Max_Size_In_Storage_Elements,
         Alignment => Allocation_Type'Alignment);

      return Convert (Location);
   end Allocation;

   --------------------------------------------------------------

   function Initialized_Allocation
     (Pool : access Dynamic_Pool_With_Subpools;
      Qualified_Expression : Allocation_Type)
      return Allocation_Type_Access
   is
      Location : System.Address;

      function Convert is new Ada.Unchecked_Conversion
        (Source => System.Address,
         Target => Allocation_Type_Access);

      use type System.Storage_Elements.Storage_Offset;

   begin

      --  pragma Assert (Allocation_Type_Access'Storage_Size /= 0);
      pragma Compile_Time_Warning
        (Ada2012_Warnings,
         "In Ada 2012, this should be a precondition");

      Allocate
        (Pool => Pool.all,
         Address => Location,
         Storage_Size =>
           Qualified_Expression'Size /
             System.Storage_Elements.Storage_Element'Size,
         Alignment => Allocation_Type'Alignment);

      declare
         Result : constant Allocation_Type_Access := Convert (Location);
      begin
         Result.all := Qualified_Expression;
         return Result;
      end;

   end Initialized_Allocation;

begin
   if Initialize_Status /= 0 then
      raise Program_Error;
   end if;
end Dynamic_Pools.Subpools;
