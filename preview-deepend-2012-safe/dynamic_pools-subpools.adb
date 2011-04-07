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

   procedure Finalize_Storage (Subpool : in out Subpool_Access);
   procedure Free_Storage_Element (Position : Storage_Vector.Cursor);

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   procedure Free_Subpool is new Ada.Unchecked_Deallocation
     (Object => Root_Subpool'Class,
      Name => Subpool_Access);

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool         : in out Dynamic_Pool_With_Subpools;
      Address      : out System.Address;
      Storage_Size : System.Storage_Elements.Storage_Count;
      Alignment    : System.Storage_Elements.Storage_Count)
   is
      Default_Subpool : Dynamic_Unbounded_Subpool_Handle
        (Mode => Auto_Unchecked_Deallocation,
         Declaring_Task_Is_Owner => True)
        := Dynamic_Unbounded_Subpool_Handle
          (Default_Subpool_for_Pool (Pool));

   begin

      Pool.Allocate_From_Subpool
        (Storage_Address => Address,
         Size_In_Storage_Elements => Storage_Size,
         Alignment => Alignment,
         Subpool => Default_Subpool);

   end Allocate;

   --------------------------------------------------------------

   overriding procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : in out Subpool_Handle'Class)
   is
      pragma Unreferenced (Alignment, Pool);
      use type System.Storage_Elements.Storage_Offset;
      use type Ada.Containers.Count_Type;
      Sub : Dynamic_Unbounded_Subpool renames
        Dynamic_Unbounded_Subpool (Subpool.Get_Access.all);
   begin

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements <
        Sub.Active'Length - Sub.Next_Allocation then

         Sub.Used_List.Append (New_Item => Sub.Active);

         if Sub.Free_List.Length > 0 and then
           Sub.Free_List.First_Element'Length >= Size_In_Storage_Elements then
            Sub.Active := Sub.Free_List.First_Element;
            Sub.Free_List.Delete_First;
         else
            Sub.Active := new System.Storage_Elements.Storage_Array
              (1 .. System.Storage_Elements.Storage_Count'Max
                 (Size_In_Storage_Elements, Minimum_Memory_Allocation));
         end if;

         Sub.Next_Allocation := Sub.Active'First;

      end if;

      Storage_Address := Sub.Active (Sub.Next_Allocation)'Address;
      Sub.Next_Allocation := Sub.Next_Allocation + Size_In_Storage_Elements;

   end Allocate_From_Subpool;

   --------------------------------------------------------------

   overriding function Create_Subpool
     (Pool : access Dynamic_Pool_With_Subpools;
      Storage_Size : Storage_Elements.Storage_Count :=
        Storage_Elements.Storage_Count'Last) return Subpool_Handle'Class
   is
      pragma Unreferenced (Storage_Size);
   begin
      return New_Handle : Dynamic_Unbounded_Subpool_Handle
        := Dynamic_Unbounded_Subpool_Handle'
          (Dynamic_Subpool_Handle with Mode => Pool.Mode,
           Declaring_Task_Is_Owner => Pool.Declaring_Task_Is_Owner)
      do
         declare
            New_Subpool : constant Subpool_Access :=
              new Dynamic_Unbounded_Subpool'
                (Dynamic_Subpool with Used_List => <>,
                 Free_List => <>,
                 Active => new System.Storage_Elements.Storage_Array
                   (1 .. Minimum_Memory_Allocation),
                 Next_Allocation => 1,
                 Owner => Ada.Task_Identification.Current_Task,
                 Parent => Pool.Default_Subpool,
                 Subpools => <>,
                 Deallocate_Storage => False);
         begin

            New_Handle.Attach (New_Subpool);

            Subpool_Vector.Append
              (Dynamic_Unbounded_Subpool
                 (Pool.Default_Subpool.all).Subpools, New_Subpool);

            Set_Pool_of_Subpool
              (Subpool => New_Handle,
               To => Pool);
         end;
      end return;

   end Create_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Parent : access Dynamic_Pool_With_Subpools)
      return Dynamic_Pool_With_Subpools is
   begin

      return New_Pool : Dynamic_Pool_With_Subpools
        := Dynamic_Pool_With_Subpools'
          (Dynamic_Pool with
           Mode => Parent.Mode,
           Declaring_Task_Is_Owner => Parent.Declaring_Task_Is_Owner,
           Default_Subpool => null)
      do
         declare
            New_Subpool : constant Subpool_Access :=
              new Dynamic_Unbounded_Subpool'
                (Dynamic_Subpool with Used_List => <>,
                 Free_List => <>,
                 Active => new System.Storage_Elements.Storage_Array
                   (1 .. Minimum_Memory_Allocation),
                 Next_Allocation => 1,
                 Owner => Ada.Task_Identification.Current_Task,
                 Parent => Parent.Default_Subpool,
                 Subpools => <>,
                 Deallocate_Storage => False);
         begin

            New_Pool.Default_Subpool := New_Subpool;

            Subpool_Vector.Append
              (Dynamic_Unbounded_Subpool
                 (Parent.Default_Subpool.all).Subpools, New_Subpool);

         end;
      end return;

   end Create_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Parent : access Dynamic_Unbounded_Subpool_Handle;
      Storage_Size : Storage_Elements.Storage_Count :=
        Storage_Elements.Storage_Count'Last) return Subpool_Handle'Class
   is
      pragma Unreferenced (Storage_Size);
   begin

      return New_Handle : Dynamic_Unbounded_Subpool_Handle
        := Dynamic_Unbounded_Subpool_Handle'
          (Dynamic_Subpool_Handle with Mode => Parent.Mode,
           Declaring_Task_Is_Owner => Parent.Declaring_Task_Is_Owner)
      do
         declare
            New_Subpool : constant Subpool_Access :=
              new Dynamic_Unbounded_Subpool'
                (Dynamic_Subpool with Used_List => <>,
                 Free_List => <>,
                 Active => new System.Storage_Elements.Storage_Array
                   (1 .. Minimum_Memory_Allocation),
                 Next_Allocation => 1,
                 Owner => Ada.Task_Identification.Current_Task,
                 Parent => Parent.Get_Access,
                 Subpools => <>,
                 Deallocate_Storage => False);
         begin

            New_Handle.Attach (New_Subpool);

            Subpool_Vector.Append
              (Dynamic_Unbounded_Subpool
                 (Parent.Get_Access.all).Subpools, New_Subpool);

            Set_Pool_of_Subpool
              (Subpool => New_Handle,
               To => Pool_of_Subpool (Parent.all));
         end;
      end return;

   end Create_Subpool;

   --------------------------------------------------------------

   overriding procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool_With_Subpools;
      Subpool : in out Subpool_Handle'Class)
   is
      pragma Unreferenced (Pool);
      Dynamic_Subpool : Dynamic_Unbounded_Subpool
        renames Dynamic_Unbounded_Subpool (Subpool.Get_Access.all);
      Position : Subpool_Vector.Cursor;

      pragma Warnings (Off, "*Subpool*modified*but*never*referenced*");

      procedure Finalize_Subpool (Position : Subpool_Vector.Cursor) is
         Subpool : Subpool_Access := Subpool_Vector.Element (Position);
      begin
         Finalize_Storage (Subpool);
      end Finalize_Subpool;
      pragma Warnings (On, "*Subpool*modified*but*never*referenced*");

      The_Subpool : Subpool_Access := Subpool.Get_Access;
   begin

      if Dynamic_Subpool.Deallocate_Storage then

         Position := Subpool_Vector.Find
           (Dynamic_Unbounded_Subpool
              (Dynamic_Subpool.Parent.all).Subpools,
            Subpool.Get_Access);

         Subpool_Vector.Delete
           (Dynamic_Unbounded_Subpool
              (Dynamic_Subpool.Parent.all).Subpools, Position);

         Finalize_Storage (The_Subpool);

         Free_Subpool (The_Subpool);
      else
         --  If we had access to the Ada finalization functionality, we would
         --  have stored called that functionality here to finalize objects
         --  needing finalization.
         --  Since we don't have access to this functionality in Ada 2005,
         --  all we can do is call Clear.

         Dynamic_Subpool.Subpools.Iterate (Process => Finalize_Subpool'Access);
         Dynamic_Subpool.Subpools.Clear;
         Dynamic_Subpool.Free_List.Append
           (New_Item => Dynamic_Subpool.Used_List);
         Dynamic_Subpool.Used_List.Clear;
         Dynamic_Subpool.Next_Allocation := 1;

      end if;

   end Deallocate_Subpool;

   --------------------------------------------------------------

   function Default_Subpool_for_Pool
     (Pool : Dynamic_Pool_With_Subpools)
      return Subpool_Handle'Class is
   begin
      return Subpool : Dynamic_Unbounded_Subpool_Handle
        := Dynamic_Unbounded_Subpool_Handle'
          (Dynamic_Subpool_Handle with Mode => Pool.Mode,
           Declaring_Task_Is_Owner => Pool.Declaring_Task_Is_Owner)
      do
         Attach (Handle => Subpool,
                 To     => Pool.Default_Subpool);
      end return;
   end Default_Subpool_for_Pool;

   --------------------------------------------------------------

   procedure Finalize_Storage (Subpool : in out Subpool_Access)
   is
      procedure Finalize_Subpool (Position : Subpool_Vector.Cursor) is
         Subpool : Subpool_Access := Subpool_Vector.Element (Position);
      begin
         if Subpool /= null then
            Finalize_Storage (Subpool);
         end if;
      end Finalize_Subpool;

      Dynamic_Subpool : Dynamic_Unbounded_Subpool renames
        Dynamic_Unbounded_Subpool (Subpool.all);

   begin
      Dynamic_Subpool.Subpools.Iterate (Process => Finalize_Subpool'Access);
      Dynamic_Subpool.Subpools.Clear;
      Dynamic_Subpool.Used_List.Iterate
        (Process => Free_Storage_Element'Access);
      Dynamic_Subpool.Used_List.Clear;
      Dynamic_Subpool.Free_List.Iterate
        (Process => Free_Storage_Element'Access);
      Dynamic_Subpool.Free_List.Clear;
      Free_Storage_Array (Dynamic_Subpool.Active);

      Subpool.Detach;

      Free_Subpool (Subpool);

   end Finalize_Storage;

   overriding procedure Finalize
     (Pool : in out Dynamic_Pool_With_Subpools)
   is
      use type Ada.Containers.Count_Type;
   begin
      if Pool.Default_Subpool = null then
         return;
      end if;

      --  Subpools dont get finalized until the top level parent is finalized

      if Dynamic_Unbounded_Subpool
        (Pool.Default_Subpool.all).Parent = null then
         Finalize_Storage (Pool.Default_Subpool);
      end if;

   end Finalize;

   --------------------------------------------------------------

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor) is
       Storage : Storage_Array_Access := Storage_Vector.Element (Position);
   begin
      Free_Storage_Array (Storage);
   end Free_Storage_Element;

   --------------------------------------------------------------

   overriding function Has_Allocations
     (Pool : Dynamic_Pool_With_Subpools) return Boolean
   is
      Dynamic_Subpool : Dynamic_Unbounded_Subpool renames
        Dynamic_Unbounded_Subpool (Pool.Default_Subpool.all);
      use type Ada.Containers.Count_Type;
      use type System.Storage_Elements.Storage_Offset;
   begin
      return Pool.Default_Subpool /= null and then not
        (Dynamic_Subpool.Used_List.Length > 0 or else
           (Dynamic_Subpool.Used_List.Length = 0 and then
            Dynamic_Subpool.Next_Allocation > 1));
   end Has_Allocations;

   --  pragma Postcondition (not Objects_Need_Finalization (Pool));
   --  Returns true if there are currently objects allocated from the pool.
   --  This is class-wide because we don't want derivations to override this
   --  function possibly changing the effect of this call.

   --------------------------------------------------------------

   overriding function Has_Allocations
     (Subpool : Dynamic_Unbounded_Subpool_Handle) return Boolean
   is
      Dynamic_Subpool : Dynamic_Unbounded_Subpool renames
        Dynamic_Unbounded_Subpool (Subpool.Get_Access.all);
      use type Ada.Containers.Count_Type;
      use type System.Storage_Elements.Storage_Offset;
   begin
      return Subpool.Get_Access /= null and then not
        (Dynamic_Subpool.Used_List.Length > 0 or else
           (Dynamic_Subpool.Used_List.Length = 0 and then
            Dynamic_Subpool.Next_Allocation > 1));
   end Has_Allocations;

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

      Pool.Default_Subpool := new Dynamic_Unbounded_Subpool'
        (Dynamic_Subpool with
         Used_List => <>,
         Free_List => <>,
         Active => new System.Storage_Elements.Storage_Array
           (1 .. Minimum_Memory_Allocation),
         Next_Allocation => 1,
         Owner => Get_Owner,
         Parent => null,
         Subpools => <>,
         Deallocate_Storage => False);

   end Initialize;

   --------------------------------------------------------------

   function Is_A_Subpool
     (Pool : Dynamic_Pool_With_Subpools'Class) return Boolean is
   begin
      return Dynamic_Unbounded_Subpool
        (Pool.Default_Subpool.all).Parent /= null;
   end Is_A_Subpool;

   --------------------------------------------------------------

   function Is_Ancestor
     (Ancestor, Child : Dynamic_Pool_With_Subpools) return Boolean
   is
      Pool : Subpool_Access := Dynamic_Unbounded_Subpool
        (Child.Default_Subpool.all).Parent;
   begin
      while Pool /= null loop
         if Pool = Ancestor.Default_Subpool then
            return True;
         end if;
         Pool := Dynamic_Unbounded_Subpool (Pool.all).Parent;
      end loop;

      return False;
   end Is_Ancestor;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Dynamic_Pool_With_Subpools;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Dynamic_Unbounded_Subpool (Pool.Default_Subpool.all).Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   function Is_Owner
     (Subpool : Dynamic_Unbounded_Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Dynamic_Unbounded_Subpool (Subpool.Get_Access.all).Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   function Pool_Needs_Finalization
     (Pool : Dynamic_Pool_With_Subpools) return Boolean
   is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return (Pool.Default_Subpool /= null);
   end Pool_Needs_Finalization;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Dynamic_Pool_With_Subpools;
      T : Task_Id := Current_Task) is
   begin
      Dynamic_Unbounded_Subpool (Pool.Default_Subpool.all).Owner := T;
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

--        Default_Subpool : Subpool_Handle'Class
--          := Pool.Default_Subpool_for_Pool;

   begin
      --  Set the flag that prevents the subpool storage from being freed. Only
      --  the objects will be freed.
      Dynamic_Unbounded_Subpool
        (Pool.Default_Subpool.all).Deallocate_Storage := False;

      --  As per AI05-0111-3
      --  Ada.Unchecked_Deallocate_Subpool (Subpool => Default_Subpool);

   end Unchecked_Deallocate_Objects;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Objects
     (Subpool : in out Dynamic_Unbounded_Subpool_Handle) is
   begin

      --  Set the flag that prevents the subpool storage from being freed. Only
      --  the objects will be freed.
      Dynamic_Unbounded_Subpool
        (Subpool.Get_Access.all).Deallocate_Storage := False;

      --  As per AI05-0111-3
      --  Ada.Unchecked_Deallocate_Subpool (Subpool => Subpool);

   end Unchecked_Deallocate_Objects;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Storage
     (Pool : in out Dynamic_Pool_With_Subpools) is

   begin

      if Pool.Default_Subpool = null then
         return;
      end if;

      --  Set the flag that prevents the subpool storage from being freed. Only
      --  the objects will be freed.
      Dynamic_Unbounded_Subpool
        (Pool.Default_Subpool.all).Deallocate_Storage := False;

      --  As per AI05-0111-3
      --  Ada.Unchecked_Deallocate_Subpool (Subpool => Default_Subpool);

   end Unchecked_Deallocate_Storage;

   procedure Unchecked_Deallocate_Storage
     (Subpool : in out Dynamic_Unbounded_Subpool_Handle) is
   begin
      if Subpool.Get_Access = null then
         return;
      end if;

      --  Set the flag that prevents the subpool storage from being freed. Only
      --  the objects will be freed.
      Dynamic_Unbounded_Subpool
        (Subpool.Get_Access.all).Deallocate_Storage := False;

      --  As per AI05-0111-3
      --  Ada.Unchecked_Deallocate_Subpool (Subpool => Subpool);

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

   function Subpool_Needs_Finalization
     (Subpool : Dynamic_Unbounded_Subpool_Handle) return Boolean is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return (Subpool.Get_Access /= null);
   end Subpool_Needs_Finalization;

end Dynamic_Pools.Subpools;
