------------------------------------------------------------------------------
--                                                                          --
--                   Deepend - Dynamic Pools for Ada 2005                   --
--                                                                          --
--                  B A S I C   D Y N A M I C   P O O L S                         --
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

package body Basic_Dynamic_Pools is

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor);

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   --------------------------------------------------------------

   overriding
   procedure Allocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Alignment);
      use type System.Storage_Elements.Storage_Count;
      use type System.Storage_Elements.Storage_Offset;
      use type Ada.Containers.Count_Type;
   begin

      if Pool.Block_Size = 0 then
         raise Program_Error;
      end if;

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements >
        Pool.Active'Length - Pool.Next_Allocation then

         Pool.Used_List.Append (New_Item => Pool.Active);

         if Pool.Free_List.Length > 0 and then
           Pool.Free_List.First_Element'Length >= Size_In_Storage_Elements then
            Pool.Active := Pool.Free_List.First_Element;
            Pool.Free_List.Delete_First;
         else
            Pool.Active := new System.Storage_Elements.Storage_Array
              (1 .. Storage_Elements.Storage_Count'Max
                 (Size_In_Storage_Elements, Pool.Block_Size));
         end if;

         Pool.Next_Allocation := Pool.Active'First;

      end if;

      Storage_Address := Pool.Active (Pool.Next_Allocation)'Address;
      Pool.Next_Allocation := Pool.Next_Allocation + Size_In_Storage_Elements;
   end Allocate;

   --------------------------------------------------------------

   overriding
   procedure Finalize   (Pool : in out Basic_Dynamic_Pool) is
   begin
      Pool.Used_List.Iterate (Process => Free_Storage_Element'Access);
      Pool.Free_List.Iterate (Process => Free_Storage_Element'Access);
      Free_Storage_Array (Pool.Active);
   end Finalize;

   --------------------------------------------------------------

   procedure Free_Storage_Element (Position : Storage_Vector.Cursor) is
       Storage : Storage_Array_Access := Storage_Vector.Element (Position);
   begin
      Free_Storage_Array (Storage);
   end Free_Storage_Element;

   --------------------------------------------------------------

   overriding procedure Initialize (Pool : in out Basic_Dynamic_Pool)
   is
      use type System.Storage_Elements.Storage_Count;
   begin
      Pool.Active := new System.Storage_Elements.Storage_Array
        (1 .. Pool.Block_Size);
      Pool.Next_Allocation := 1;
      Pool.Owner := Ada.Task_Identification.Current_Task;
   end Initialize;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) is
   begin
      Pool.Owner := T;
   end Set_Owner;

   --------------------------------------------------------------

   overriding
   function Storage_Size
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count is
      pragma Unreferenced (Pool);
   begin
      return Storage_Elements.Storage_Count'Last;
   end Storage_Size;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Objects
     (Pool : in out Basic_Dynamic_Pool) is
   --  Currently only the vendor could supply such a routine. Ada 2012
   --  will expose this functionality, but for now we have to say that
   --  it is erroneous to allocate objects needing finalization such as tasks,
   --  protected objects and type derived types defined in Ada.Finalization
   --  from a dynamic pool.

   begin
      Pool.Free_List.Append (New_Item => Pool.Used_List);
      Pool.Used_List.Clear;
      Pool.Next_Allocation := 1;
   end Unchecked_Deallocate_Objects;

end Basic_Dynamic_Pools;
