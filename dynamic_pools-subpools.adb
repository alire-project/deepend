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
with Ada.Unchecked_Deallocation;

package body Dynamic_Pools.Subpools is

   Minimum_Memory_Allocation : constant := 4_096;

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor);

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   procedure Free_Storage is new Ada.Unchecked_Deallocation
     (Object => Pool_Storage,
      Name => Pool_Storage_Access);

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
      use type System.Storage_Elements.Storage_Offset;
      use type Ada.Containers.Count_Type;
   begin

      --  If there's not enough space in the current hunk of memory
      if Storage_Size <
        Pool.Storage.Active'Length - Pool.Storage.Next_Allocation then

         Pool.Storage.Used_List.Append (New_Item => Pool.Storage.Active);

         if Pool.Storage.Free_List.Length > 0 and then
           Pool.Storage.Free_List.First_Element'Length >= Storage_Size then
            Pool.Storage.Active := Pool.Storage.Free_List.First_Element;
            Pool.Storage.Free_List.Delete_First;
         else
            Pool.Storage.Active := new System.Storage_Elements.Storage_Array
              (1 .. System.Storage_Elements.Storage_Count'Max
                 (Storage_Size, Minimum_Memory_Allocation));
         end if;

         Pool.Storage.Next_Allocation := Pool.Storage.Active'First;

      end if;

      Address := Pool.Storage.Active (Pool.Storage.Next_Allocation)'Address;
      Pool.Storage.Next_Allocation :=
        Pool.Storage.Next_Allocation + Storage_Size;

   end Allocate;

   --------------------------------------------------------------

   function Create_Subpool
     (Parent : access Dynamic_Pool_With_Subpools)
      return Dynamic_Pool_With_Subpools is
   begin

      return  New_Pool : Dynamic_Pool_With_Subpools
        := (Dynamic_Pool with
            Mode => Auto_Unchecked_Deallocation,
            Declaring_Task_Is_Owner => True,
            Storage => new Pool_Storage'
              (Used_List => <>,
               Free_List => <>,
               Active => new System.Storage_Elements.Storage_Array
                 (1 .. Minimum_Memory_Allocation),
               Next_Allocation => 1,
               Owner => Ada.Task_Identification.Current_Task,
               Parent => Parent.Storage,
               Subpools => <>,
               Object => null))
      do
         Subpool_Vector.Append
           (Parent.Storage.Subpools, New_Pool.Storage);

         New_Pool.Storage.Object := New_Pool.Storage'Unchecked_Access;
      end return;

   end Create_Subpool;

   --------------------------------------------------------------

   procedure Finalize_Storage (Storage : in out Pool_Storage_Access)
   is
      procedure Finalize_Subpool (Position : Subpool_Vector.Cursor) is
         Subpool : Pool_Storage_Access := Subpool_Vector.Element (Position);
      begin
         if Subpool /= null then
            Finalize_Storage (Subpool);
         end if;
      end Finalize_Subpool;

   begin
      Storage.Subpools.Iterate (Process => Finalize_Subpool'Access);
      Storage.Subpools.Clear;
      Storage.Used_List.Iterate (Process => Free_Storage_Element'Access);
      Storage.Used_List.Clear;
      Storage.Free_List.Iterate (Process => Free_Storage_Element'Access);
      Storage.Free_List.Clear;
      Free_Storage_Array (Storage.Active);

      if Storage.Object /= null then
         Storage.Object := null;
      end if;

      Free_Storage (Storage);

   end Finalize_Storage;

   overriding procedure Finalize
     (Pool : in out Dynamic_Pool_With_Subpools)
   is
      use type Ada.Containers.Count_Type;
   begin
      if Pool.Storage = null then
         return;
      end if;

      --  Pool storage is now detached from its object
      Pool.Storage.Object := null;

      --  Subpools dont get finalized until the top level parent is finalized

      if Pool.Storage.Parent = null then
         Finalize_Storage (Pool.Storage);
      end if;

   end Finalize;

   --------------------------------------------------------------

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor) is
       Storage : Storage_Array_Access := Storage_Vector.Element (Position);
   begin
      Free_Storage_Array (Storage);
   end Free_Storage_Element;

   --------------------------------------------------------------

   overriding procedure Initialize
     (Pool : in out Dynamic_Pool_With_Subpools)
   is
      function Get_Owner return Ada.Task_Identification.Task_Id is
      begin
         if Pool.Declaring_Task_Is_Owner then
            return Ada.Task_Identification.Current_Task;
         else
            return Ada.Task_Identification.Null_Task_Id;
         end if;
      end Get_Owner;

   begin

      Pool.Storage := new Pool_Storage'
        (Used_List => <>,
         Free_List => <>,
         Active => new System.Storage_Elements.Storage_Array
           (1 .. Minimum_Memory_Allocation),
         Next_Allocation => 1,
         Owner => Get_Owner,
         Parent => null,
         Subpools => <>,
         Object => Pool.Storage'Unchecked_Access);

   end Initialize;

   --------------------------------------------------------------

   function Is_A_Subpool
     (Pool : Dynamic_Pool_With_Subpools'Class) return Boolean is
   begin
      return Pool.Storage.Parent /= null;
   end Is_A_Subpool;

   --------------------------------------------------------------

   function Is_Ancestor
     (Ancestor, Child : Dynamic_Pool_With_Subpools) return Boolean
   is
      Pool : Pool_Storage_Access := Child.Storage.Parent;
   begin
      while Pool /= null loop
         if Pool = Ancestor.Storage then
            return True;
         end if;
         Pool := Pool.Parent;
      end loop;

      return False;
   end Is_Ancestor;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Dynamic_Pool_With_Subpools;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Storage.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   function Pool_Needs_Finalization
     (Pool : Dynamic_Pool_With_Subpools) return Boolean
   is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return (Pool.Storage /= null);
   end Pool_Needs_Finalization;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Dynamic_Pool_With_Subpools;
      T : Task_Id := Current_Task) is
   begin
      Pool.Storage.Owner := T;
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
     (Pool : in out Dynamic_Pool_With_Subpools)
   is
      pragma Warnings (Off, "*Subpool*modified*but*never*referenced*");
      procedure Finalize_Subpool (Position : Subpool_Vector.Cursor) is
         Subpool : Pool_Storage_Access := Subpool_Vector.Element (Position);
      begin
         Finalize_Storage (Subpool);
      end Finalize_Subpool;
      pragma Warnings (On, "*Subpool*modified*but*never*referenced*");
   begin
      --  If we had access to the Ada finalization functionality, we would
      --  have stored called that functionality here to finalize objects
      --  needing finalization.
      --  Since we don't have access to this functionality in Ada 2005, all we
      --  can do is call Clear.

      Pool.Storage.Subpools.Iterate (Process => Finalize_Subpool'Access);
      Pool.Storage.Subpools.Clear;
      Pool.Storage.Free_List.Append (New_Item => Pool.Storage.Used_List);
      Pool.Storage.Used_List.Clear;
      Pool.Storage.Next_Allocation := 1;
   end Unchecked_Deallocate_Objects;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Storage
     (Pool : in out Dynamic_Pool_With_Subpools) is
   begin

      if Pool.Storage = null then
         return;
      end if;

      if Pool.Storage.Parent /= null then
         declare
            Position : Subpool_Vector.Cursor := Subpool_Vector.Find
              (Pool.Storage.Parent.Subpools, Pool.Storage);
         begin
            Subpool_Vector.Delete
              (Pool.Storage.Parent.Subpools, Position);
         end;
      end if;

      Finalize_Storage (Pool.Storage);

   end Unchecked_Deallocate_Storage;

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
end Dynamic_Pools.Subpools;
