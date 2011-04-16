------------------------------------------------------------------------------
--                                                                          --
--                   Deepend - Dynamic Pools for Ada 2005                   --
--                                                                          --
--                        D Y N A M I C   P O O L S                         --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                  Copyright (C) 2011, Bradley J. Moore                    --
--                                                                          --
--  Deepend is free software;  you can  redistribute it  and/or modify it   --
--  under  terms of the  GNU General Public License  as  published  by the  --
--  Free Software  Foundation;  either version 2,  or (at your option) any  --
--  later  version.  Paraffin is  distributed in the hope that it  will be  --
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of  --
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU  --
--  General Public License for  more details.  You should have  received a  --
--  copy of the GNU General Public License distributed with Deepend;  see  --
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

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Dynamic_Pools is

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor);

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   procedure Free_Subpool is new Ada.Unchecked_Deallocation
     (Object => Dynamic_Subpool,
      Name => Dynamic_Subpool_Access);

   protected body Subpool_Set is

      procedure Add (Subpool : Dynamic_Subpool_Access) is
      begin
         Subpools.Append (Subpool);
      end Add;

      --------------------------------------------------------------

      procedure Deallocate_All
      is
         procedure Deallocate_Pools (Position : Subpool_Vector.Cursor) is
            Subpool : Dynamic_Subpool_Access :=
              Subpool_Vector.Element (Position);
         begin

            Subpool.Used_List.Iterate
              (Process => Free_Storage_Element'Access);

            Subpool.Free_List.Iterate
              (Process => Free_Storage_Element'Access);

            Free_Storage_Array (Subpool.Active);

            Free_Subpool (Subpool);
         end Deallocate_Pools;
      begin
         Subpools.Iterate (Deallocate_Pools'Access);
      end Deallocate_All;

      --------------------------------------------------------------

      procedure Delete (Subpool : Dynamic_Subpool_Access) is
         Position : Subpool_Vector.Cursor;
      begin
         Position := Subpools.Find (Subpool);
         Subpools.Delete (Position);
      end Delete;

   end Subpool_Set;

   --------------------------------------------------------------

   overriding
   procedure Allocate
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      use type System.Storage_Elements.Storage_Count;
   begin
      if Pool.Default_Block_Size = 0 then
         raise Storage_Error;
      end if;

      Pool.Allocate_From_Subpool
        (Storage_Address,
         Size_In_Storage_Elements,
         Alignment,
         Pool.Default_Subpool_for_Pool);
   end Allocate;

   overriding
   procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : --  not null
      Subpool_Handle)
   is
      pragma Unreferenced (Alignment, Pool);
      use type System.Storage_Elements.Storage_Offset;
      use type Ada.Containers.Count_Type;
      Sub : Dynamic_Subpool renames Dynamic_Subpool (Subpool.all);
   begin

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements >
        Sub.Active'Length - Sub.Next_Allocation then

         Sub.Used_List.Append (New_Item => Sub.Active);

         if Sub.Free_List.Length > 0 and then
           Sub.Free_List.First_Element'Length >= Size_In_Storage_Elements then
            Sub.Active := Sub.Free_List.First_Element;
            Sub.Free_List.Delete_First;
         else
            Sub.Active := new System.Storage_Elements.Storage_Array
              (1 .. Storage_Elements.Storage_Count'Max
                 (Size_In_Storage_Elements, Sub.Block_Size));
         end if;

         Sub.Next_Allocation := Sub.Active'First;

      end if;

      Storage_Address := Sub.Active (Sub.Next_Allocation)'Address;
      Sub.Next_Allocation := Sub.Next_Allocation + Size_In_Storage_Elements;
   end Allocate_From_Subpool;

   --------------------------------------------------------------

   function Allocation
     (Subpool : Subpool_Handle) return Allocation_Type_Access
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

      Pool_of_Subpool (Subpool).Allocate_From_Subpool
        (Storage_Address => Location,
         Size_In_Storage_Elements =>
           Allocation_Type'Max_Size_In_Storage_Elements,
         Alignment => Allocation_Type'Alignment,
         Subpool => Subpool);

      return Convert (Location);
   end Allocation;

   --------------------------------------------------------------

   overriding
   function Create_Subpool
     (Pool : access Dynamic_Pool) return not null Subpool_Handle
   is
      use type System.Storage_Elements.Storage_Count;
   begin
      if Pool.Default_Block_Size = 0 then
         return Create_Subpool (Pool, Default_Allocation_Block_Size);
      else
         return Create_Subpool (Pool, Pool.Default_Block_Size);
      end if;
   end Create_Subpool;

   not overriding
   function Create_Subpool
     (Pool : access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count)
      return not null Subpool_Handle
   is
      New_Pool : constant Dynamic_Subpool_Access
        := new Dynamic_Subpool'
          (Root_Subpool with
           Block_Size => Block_Size,
           Used_List => <>,
           Free_List => <>,
           Active => new System.Storage_Elements.Storage_Array
             (1 .. Block_Size),
           Next_Allocation => 1,
           Owner => Ada.Task_Identification.Current_Task,
           Deallocate_Storage => True);

      Result : constant Subpool_Handle := New_Pool.all'Unchecked_Access;
   begin

      Pool.Subpools.Add (New_Pool);

      Set_Pool_of_Subpool
        (Subpool => Result,
         To => Pool.all);

      return Result;

   end Create_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Pool : access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count :=
        Default_Allocation_Block_Size) return Scoped_Subpool_Handle
   is
      New_Subpool : constant Subpool_Handle :=
        Create_Subpool (Pool, Block_Size);
   begin
      pragma Warnings (Off, "*Result*is not referenced*");
      return  Result : Scoped_Subpool_Handle (Handle => New_Subpool);
      pragma Warnings (On, "*Result*is not referenced*");
   end Create_Subpool;

   --------------------------------------------------------------

   overriding
   procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool;
      Subpool : in out Subpool_Handle)
   is
      The_Subpool : Dynamic_Subpool_Access
        := Dynamic_Subpool (Subpool.all)'Access;
   begin

      if The_Subpool.Deallocate_Storage then

         Pool.Subpools.Delete (The_Subpool);

         The_Subpool.Used_List.Iterate
           (Process => Free_Storage_Element'Access);

         The_Subpool.Used_List.Clear;

         The_Subpool.Free_List.Iterate
           (Process => Free_Storage_Element'Access);

         The_Subpool.Free_List.Clear;
         Free_Storage_Array (The_Subpool.Active);

         Free_Subpool (The_Subpool);
      else
         --  If we had access to the Ada finalization functionality, we would
         --  have stored called that functionality here to finalize objects
         --  needing finalization.
         --  Since we don't have access to this functionality in Ada 2005,
         --  all we can do is call Clear.

         The_Subpool.Free_List.Append
           (New_Item => The_Subpool.Used_List);

         The_Subpool.Used_List.Clear;
         The_Subpool.Next_Allocation := 1;
         The_Subpool.Deallocate_Storage := False;

      end if;

   end Deallocate_Subpool;

   --------------------------------------------------------------

   overriding
   function Default_Subpool_for_Pool
     (Pool : Dynamic_Pool)
      return not null Subpool_Handle is
   begin
      return Pool.Default_Subpool;
   end Default_Subpool_for_Pool;

   --------------------------------------------------------------

   overriding
   procedure Finalize   (Pool : in out Dynamic_Pool) is
   begin
      Pool.Subpools.Deallocate_All;
   end Finalize;

   --------------------------------------------------------------

   package body Scoped_Subpools is
      overriding
      procedure Finalize (Scoped_Subpool : in out Scoped_Subpool_Handle) is
         Subpool : Subpool_Handle := Scoped_Subpool.Handle;
      begin
         Unchecked_Deallocate_Subpool (Subpool);
      end Finalize;
   end Scoped_Subpools;

   --------------------------------------------------------------

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor) is
       Storage : Storage_Array_Access := Storage_Vector.Element (Position);
   begin
      Free_Storage_Array (Storage);
   end Free_Storage_Element;

   --------------------------------------------------------------

   overriding procedure Initialize (Pool : in out Dynamic_Pool)
   is
      use type System.Storage_Elements.Storage_Count;
   begin
      if Pool.Default_Block_Size > 0 then
         Pool.Default_Subpool := Pool.Create_Subpool;
      else
         Pool.Default_Subpool := null;
      end if;
   end Initialize;

   --------------------------------------------------------------

   function Initialized_Allocation
     (Subpool : Subpool_Handle;
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

      Pool_of_Subpool (Subpool).Allocate_From_Subpool
        (Storage_Address => Location,
         Size_In_Storage_Elements =>
           Qualified_Expression'Size /
             System.Storage_Elements.Storage_Element'Size,
         Alignment => Allocation_Type'Alignment,
         Subpool => Subpool);

      declare
         Result : constant Allocation_Type_Access := Convert (Location);
      begin
         Result.all := Qualified_Expression;
         return Result;
      end;

   end Initialized_Allocation;

   --------------------------------------------------------------

   function Is_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Dynamic_Subpool (Subpool.all).Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   procedure Set_Owner
     (Subpool : not null Subpool_Handle;
      T : Task_Id := Current_Task) is
   begin
      Dynamic_Subpool (Subpool.all).Owner := T;
   end Set_Owner;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Objects
     (Subpool : Subpool_Handle) is
   --  Currently only the vendor could supply such a routine. Ada 2012
   --  will expose this functionality, but for now we have to say that
   --  it is erroneous to allocate objects needing finalization such as tasks,
   --  protected objects and type derived types defined in Ada.Finalization
   --  from a dynamic pool.

      Copy : Subpool_Handle := Subpool;

   begin

      --  Set the flag that prevents the subpool storage from being freed. Only
      --  the objects will be freed.
      Dynamic_Subpool
        (Subpool.all).Deallocate_Storage := False;

      --  Since Ada.Unchecked_Deallocate_Subpool doesn't exist currently,
      --  dispatch to Deallocate_Subpool directly for now.
      Pool_of_Subpool (Subpool).Deallocate_Subpool (Copy);

      --  As per AI05-0111-3
      --  Ada.Unchecked_Deallocate_Subpool (Copy);
      --  In this case, Copy will not be set to null after this call
   end Unchecked_Deallocate_Objects;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Subpool
     (Subpool : in out Subpool_Handle) is
   begin
      if Subpool = null then
         return;
      end if;

      --  Set the flag that prevents the subpool storage from being freed. Only
      --  the objects will be freed.
      Dynamic_Subpool (Subpool.all).Deallocate_Storage := True;

      --  Since Ada.Unchecked_Deallocate_Subpool doesn't exist currently,
      --  dispatch to Deallocate_Subpool directly for now.
      Pool_of_Subpool (Subpool).Deallocate_Subpool (Subpool);

      --  As per AI05-0111-3
      --  Ada.Unchecked_Deallocate_Subpool (Subpool);

   end Unchecked_Deallocate_Subpool;

end Dynamic_Pools;
