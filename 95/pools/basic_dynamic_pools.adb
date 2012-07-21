------------------------------------------------------------------------------
--
--              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
--
--                   B A S I C   D Y N A M I C   P O O L S
--
--                                B o d y
--
--                  Copyright (C) 2011, Bradley J. Moore
--
--  Deepend is free software;  you can  redistribute it  and/or modify it
--  under  terms of the  GNU General Public License  as  published  by the
--  Free Software  Foundation;  either version 2,  or (at your option) any
--  later  version.  Paraffin is  distributed in the hope that it  will be
--  useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY  or  FITNESS  FOR  A  PARTICULAR PURPOSE.  See the GNU
--  General Public License for  more details.  You should have  received a
--  copy of the GNU General Public License distributed with Deepend;  see
--  file  COPYING.  If  not,  write  to  the  Free  Software  Foundation,
--  51 Franklin  Street,  Fifth  Floor, Boston, MA 02110-1301, USA.
--
--  As a  special exception, if other files  instantiate generics from
--  this unit,  or you link this  unit with other files  to produce an
--  executable,  this unit  does  not by  itself  cause the  resulting
--  executable to be covered by  the GNU General Public License.  This
--  exception does  not however invalidate  any other reasons  why the
--  executable file might be covered by the GNU Public License.
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Basic_Dynamic_Pools is

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   procedure Free_Vector is new Ada.Unchecked_Deallocation
     (Object => Vector,
      Name => Vector_Access);

   procedure Append (Container : in out Storage_Vector;
                     New_Item : Storage_Array_Access);

   function Last_Element
     (Container : Storage_Vector) return Storage_Array_Access;

   procedure Delete_Last (Container : in out Storage_Vector);

   function Length (Container : Storage_Vector) return Natural;

   pragma Inline (Append, Last_Element, Delete_Last, Length);

   --------------------------------------------------------------

   procedure Allocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count)
   is
      pragma Unreferenced (Alignment);
      use type Storage_Elements.Storage_Count;
   begin

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements >
        Pool.Active'Length - Pool.Next_Allocation then

         Append (Container => Pool.Used_List,
                 New_Item => Pool.Active);

         if Length (Pool.Free_List) > 0 and then
           Last_Element
             (Pool.Free_List)'Length >= Size_In_Storage_Elements then
            Pool.Active := Last_Element (Pool.Free_List);
            Delete_Last (Pool.Free_List);
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

   procedure Append (Container : in out Storage_Vector;
                     New_Item : Storage_Array_Access)
   is
      use type System.Storage_Elements.Storage_Count;
   begin
      if Container.Last = Container.Storage.all'Length then
         declare
            Old : Vector_Access := Container.Storage;
         begin
            Container.Storage := new Vector (1 .. Old'Length * 2);
            Container.Storage.all (1 .. Old'Length) := Old.all;
            Free_Vector (Old);
         end;
      end if;

      Container.Last := Container.Last + 1;
      Container.Storage (Container.Last) := New_Item;
   end Append;

   --------------------------------------------------------------

   procedure Deallocate
     (Pool : in out Basic_Dynamic_Pool;
      Storage_Address : Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin
      null;
   end Deallocate;

   --------------------------------------------------------------

   procedure Delete_Last (Container : in out Storage_Vector) is
      use type System.Storage_Elements.Storage_Count;
   begin
      Container.Last := Container.Last - 1;
   end Delete_Last;

   --------------------------------------------------------------

   procedure Finalize   (Pool : in out Basic_Dynamic_Pool) is
   begin
      for I in 1 .. Pool.Used_List.Last loop
         Free_Storage_Array (Pool.Used_List.Storage (I));
      end loop;

      Free_Vector (Pool.Used_List.Storage);

      for I in 1 .. Pool.Free_List.Last loop
         Free_Storage_Array (Pool.Free_List.Storage (I));
      end loop;

      Free_Vector (Pool.Free_List.Storage);

      Free_Storage_Array (Pool.Active);
   end Finalize;

   --------------------------------------------------------------

   procedure Initialize (Pool : in out Basic_Dynamic_Pool)
   is
      use type System.Storage_Elements.Storage_Count;
   begin
      Pool.Active := new System.Storage_Elements.Storage_Array
        (1 .. Pool.Block_Size);
      Pool.Next_Allocation := 1;
      Pool.Owner := Ada.Task_Identification.Current_Task;
      Pool.Used_List := Storage_Vector'
        (Storage => new Vector (1 .. 1024),
         Last => 1);
      Pool.Free_List := Storage_Vector'
        (Storage => new  Vector (1 .. 1024),
         Last => 1);
   end Initialize;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   function Last_Element (Container : Storage_Vector)
                           return Storage_Array_Access is
   begin
      return Container.Storage (Container.Last);
   end Last_Element;

   --------------------------------------------------------------

   function Length (Container : Storage_Vector) return Natural is
   begin
      return Natural (Container.Last);
   end Length;

   --------------------------------------------------------------

   procedure Set_Owner
     (Pool : in out Basic_Dynamic_Pool;
      T : Task_Id := Current_Task) is
   begin
      pragma Assert
        ((Is_Owner (Pool, Null_Task_Id) and then T = Current_Task)
         or else (Is_Owner (Pool) and then T = Null_Task_Id));

      Pool.Owner := T;

   end Set_Owner;

   --------------------------------------------------------------

   function Storage_Size
     (Pool : Basic_Dynamic_Pool)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;

      use type Storage_Elements.Storage_Count;
   begin
      for I in 1 .. Pool.Used_List.Last loop
         Result := Result + Pool.Used_List.Storage.all'Length;
      end loop;

      return Result + Pool.Next_Allocation - 1;
   end Storage_Size;

end Basic_Dynamic_Pools;
