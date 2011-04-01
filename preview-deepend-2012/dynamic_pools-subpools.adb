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

   procedure Finalize_Storage (Storage : in out Pool_Storage_Access);
   procedure Free_Storage_Element (Position : Storage_Vector.Cursor);

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   procedure Free_Storage is new Ada.Unchecked_Deallocation
     (Object => Pool_Storage,
      Name => Pool_Storage_Access);

   procedure Free_Subpool is new Ada.Unchecked_Deallocation
     (Object => Dynamic_Unbounded_Subpool,
      Name => Dynamic_Subpool_Handle);

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

   --  overriding
   procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool_With_Subpools;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : not null Subpool_Handle)
   is
      pragma Unreferenced (Alignment, Pool);
      use type System.Storage_Elements.Storage_Offset;
      use type Ada.Containers.Count_Type;
      Sub : Dynamic_Unbounded_Subpool renames
        Dynamic_Unbounded_Subpool (Subpool.all);
   begin

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements <
        Sub.Storage.Active'Length - Sub.Storage.Next_Allocation then

         Sub.Storage.Used_List.Append (New_Item => Sub.Storage.Active);

         if Sub.Storage.Free_List.Length > 0 and then
           Sub.Storage.Free_List.First_Element'Length >=
             Size_In_Storage_Elements then
            Sub.Storage.Active := Sub.Storage.Free_List.First_Element;
            Sub.Storage.Free_List.Delete_First;
         else
            Sub.Storage.Active := new System.Storage_Elements.Storage_Array
              (1 .. System.Storage_Elements.Storage_Count'Max
                 (Size_In_Storage_Elements, Minimum_Memory_Allocation));
         end if;

         Sub.Storage.Next_Allocation := Sub.Storage.Active'First;

      end if;

      Storage_Address :=
        Sub.Storage.Active (Sub.Storage.Next_Allocation)'Address;
      Sub.Storage.Next_Allocation := Sub.Storage.Next_Allocation +
        Size_In_Storage_Elements;

   end Allocate_From_Subpool;

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

   overriding function Create_Subpool
     (Pool : access Dynamic_Pool_With_Subpools;
      Storage_Size : Storage_Elements.Storage_Count :=
        Storage_Elements.Storage_Count'Last) return not null Subpool_Handle
   is
      pragma Unreferenced (Storage_Size);
      Result : constant Dynamic_Subpool_Handle :=
        new Dynamic_Unbounded_Subpool'
        (Dynamic_Subpool with
         Mode => Auto_Unchecked_Deallocation,
         Declaring_Task_Is_Owner => True,
         Storage => new Pool_Storage'
           (Used_List => <>,
            Free_List => <>,
            Active => new System.Storage_Elements.Storage_Array
              (1 .. Minimum_Memory_Allocation),
            Next_Allocation => 1,
            Owner => Ada.Task_Identification.Current_Task,
            Parent => Pool.Storage,
            Subpools => <>,
            Object => null));

   begin
      Subpool_Vector.Append
        (Pool.Storage.Subpools, Result.Storage);

      Result.Storage.Object := Result.Storage'Unchecked_Access;
      Set_Pool_of_Subpool
        (Subpool => Subpool_Handle (Result),
         To => Pool);
      return Subpool_Handle (Result);
   end Create_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Parent : Subpool_Handle;
      Storage_Size : Storage_Elements.Storage_Count :=
        Storage_Elements.Storage_Count'Last) return not null Subpool_Handle
   is
      pragma Unreferenced (Storage_Size);

      Parent_Pool : Dynamic_Unbounded_Subpool renames
        Dynamic_Unbounded_Subpool (Parent.all);

      Result : constant Dynamic_Subpool_Handle :=
        new Dynamic_Unbounded_Subpool'
        (Dynamic_Subpool with
         Mode => Auto_Unchecked_Deallocation,
         Declaring_Task_Is_Owner => True,
         Storage => new Pool_Storage'
           (Used_List => <>,
            Free_List => <>,
            Active => new System.Storage_Elements.Storage_Array
              (1 .. Minimum_Memory_Allocation),
            Next_Allocation => 1,
            Owner => Ada.Task_Identification.Current_Task,
            Parent => Parent_Pool.Storage,
            Subpools => <>,
            Object => null));

   begin
      Subpool_Vector.Append
        (Parent_Pool.Storage.Subpools, Result.Storage);

      Result.Storage.Object := Result.Storage'Unchecked_Access;
      Set_Pool_of_Subpool
        (Subpool => Subpool_Handle (Result),
         To => Pool_of_Subpool (Parent));
      return Subpool_Handle (Result);
   end Create_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Parent : Subpool_Handle;
      Storage_Size : Storage_Elements.Storage_Count :=
        Storage_Elements.Storage_Count'Last) return Dynamic_Unbounded_Subpool
   is
      pragma Unreferenced (Storage_Size);
      Parent_Pool : Dynamic_Unbounded_Subpool renames
        Dynamic_Unbounded_Subpool (Parent.all);
   begin

      return  New_Subpool : aliased Dynamic_Unbounded_Subpool
        := (Dynamic_Subpool with
            Mode => Auto_Unchecked_Deallocation,
            Declaring_Task_Is_Owner => True,
            Storage => new Pool_Storage'
              (Used_List => <>,
               Free_List => <>,
               Active => new System.Storage_Elements.Storage_Array
                 (1 .. Minimum_Memory_Allocation),
               Next_Allocation => 1,
               Owner => Ada.Task_Identification.Current_Task,
               Parent => Parent_Pool.Storage,
               Subpools => <>,
               Object => null))
      do
         Subpool_Vector.Append
           (Parent_Pool.Storage.Subpools, New_Subpool.Storage);

         New_Subpool.Storage.Object := New_Subpool.Storage'Unchecked_Access;
         Set_Pool_of_Subpool
           (Subpool => Subpool_Handle'(New_Subpool'Unchecked_Access),
            To => Pool_of_Subpool (Parent));
      end return;
   end Create_Subpool;

   --------------------------------------------------------------

--     function Create_Subpool
--       (Parent : Dynamic_Unbounded_Subpool;
--        Storage_Size : Storage_Elements.Storage_Count :=
--          Storage_Elements.Storage_Count'Last)
--           return Dynamic_Unbounded_Subpool
--     is
--     begin
--
--        return  New_Subpool : Dynamic_Unbounded_Subpool
--          := (Dynamic_Subpool with
--              Mode => Auto_Unchecked_Deallocation,
--              Declaring_Task_Is_Owner => True,
--              Storage => new Pool_Storage'
--                (Used_List => <>,
--                 Free_List => <>,
--                 Active => new System.Storage_Elements.Storage_Array
--                   (1 .. Minimum_Memory_Allocation),
--                 Next_Allocation => 1,
--                 Owner => Ada.Task_Identification.Current_Task,
--                 Parent => Parent.Storage,
--                 Subpools => <>,
--                 Object => null))
--        do
--           New_Subpool.Storage.Object
--             := New_Subpool.Storage'Unchecked_Access;
--           Set_Pool_of_Subpool
--             (Subpool => Subpool_Handle (Result),
--              To => Parent);
--        end return;
--     end Create_Subpool;

   --------------------------------------------------------------

   overriding procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool_With_Subpools;
      Subpool : in out Subpool_Handle)
   is
      pragma Unreferenced (Pool);
      Dynamic_Subpool : Dynamic_Subpool_Handle
        := Dynamic_Subpool_Handle (Subpool);
      Position : Subpool_Vector.Cursor;
   begin

      if Dynamic_Subpool.Storage /= null then
         Position := Subpool_Vector.Find
           (Dynamic_Subpool.Storage.Parent.Subpools, Dynamic_Subpool.Storage);

         Subpool_Vector.Delete
           (Dynamic_Subpool.Storage.Parent.Subpools, Position);

         Finalize_Storage (Dynamic_Subpool.Storage);
      end if;

      Free_Subpool (Dynamic_Subpool);

   end Deallocate_Subpool;

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

   function Is_Owner
     (Subpool : Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean
   is
      Sub : Dynamic_Unbounded_Subpool renames
        Dynamic_Unbounded_Subpool (Subpool.all);
   begin
      return (Sub.Storage.Owner = T);
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

   function Subpool_Needs_Finalization
     (Subpool : Subpool_Handle) return Boolean is
      use type System.Storage_Elements.Storage_Offset;
   begin
      return (Dynamic_Unbounded_Subpool (Subpool.all).Storage /= null);
   end Subpool_Needs_Finalization;

end Dynamic_Pools.Subpools;
