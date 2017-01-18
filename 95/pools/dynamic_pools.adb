------------------------------------------------------------------------------
--
--              Deepend - Dynamic Pools for Ada 2005 and Ada 2012
--
--                        D Y N A M I C   P O O L S
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
with System.Address_To_Access_Conversions;

package body Dynamic_Pools is

   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation
     (Object => System.Storage_Elements.Storage_Array,
      Name => Storage_Array_Access);

   procedure Free_Subpool is new Ada.Unchecked_Deallocation
     (Object => Dynamic_Subpool,
      Name => Dynamic_Subpool_Access);

   procedure Free_Vector is new Ada.Unchecked_Deallocation
     (Object => Vector,
      Name => Vector_Access);

   procedure Free_Subpool_List is new Ada.Unchecked_Deallocation
     (Object => Subpool_Array,
      Name   => Subpool_Array_Access);

   procedure Append
     (Container : in out Subpool_Vector;
      New_Item : Dynamic_Subpool_Access);

   procedure Append (Container : in out Storage_Vector;
                     New_Item : Storage_Array_Access);

   function Last_Element
     (Container : Storage_Vector) return Storage_Array_Access;

   procedure Delete_Last (Container : in out Storage_Vector);

   function Length (Container : Storage_Vector) return Natural;

   function Storage_Size
     (Subpool : Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count;

   function Storage_Used
     (Subpool : Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count;

   protected body Subpool_Set is

      procedure Add (Subpool : Dynamic_Subpool_Access) is
      begin
         Append (Subpools, Subpool);
      end Add;

      --------------------------------------------------------------

      procedure Deallocate_All is
      begin
         for I in 1 .. Subpools.Last loop

            for J in 1 .. Subpools.Subpool_List (I).Used_List.Last loop
               Free_Storage_Array
                 (Subpools.Subpool_List (I).Used_List.Storage (J));
            end loop;

            Free_Vector (Subpools.Subpool_List (I).Used_List.Storage);

            for J in 1 .. Subpools.Subpool_List (I).Free_List.Last loop
               Free_Storage_Array
                 (Subpools.Subpool_List (I).Free_List.Storage (J));
            end loop;

            Free_Vector (Subpools.Subpool_List (I).Free_List.Storage);

            Free_Storage_Array (Subpools.Subpool_List (I).Active);

            Free_Subpool (Subpools.Subpool_List (I));

         end loop;

         Free_Subpool_List (Subpools.Subpool_List);

      end Deallocate_All;

      --------------------------------------------------------------

      procedure Delete (Subpool : Dynamic_Subpool_Access) is
      begin
         Delete_Loop : for I in 1 .. Subpools.Last loop
            if Subpools.Subpool_List (I) = Subpool then
               Subpools.Subpool_List (I .. Subpools.Last - 1)
                 := Subpools.Subpool_List (I + 1 .. Subpools.Last);
               Subpools.Last := Subpools.Last - 1;
               exit Delete_Loop;
            end if;
         end loop Delete_Loop;
      end Delete;

      --------------------------------------------------------------

      procedure Initialize is
      begin
         Subpools := Subpool_Vector'
           (Subpool_List => new Subpool_Array (1 .. 1024),
            Last => 0);
      end Initialize;

      --------------------------------------------------------------

      function Storage_Total return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;

      begin
         for I in 1 .. Subpools.Last loop
            Result := Result + Storage_Size (Subpools.Subpool_List (I));
         end loop;
         return Result;
      end Storage_Total;

      --------------------------------------------------------------

      function Storage_Usage return Storage_Elements.Storage_Count
      is
         Result : Storage_Elements.Storage_Count := 0;
      begin
         for I in 1 .. Subpools.Last loop
            Result := Result + Storage_Used (Subpools.Subpool_List (I));
         end loop;

         return Result;
      end Storage_Usage;

   end Subpool_Set;

   --------------------------------------------------------------

   procedure Allocate
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count) is
   begin

      Allocate_From_Subpool
        (Pool,
         Storage_Address,
         Size_In_Storage_Elements,
         Alignment,
         Pool.Default_Subpool);

   end Allocate;

   --------------------------------------------------------------

   procedure Allocate_From_Subpool
     (Pool : in out Dynamic_Pool;
      Storage_Address : out Address;
      Size_In_Storage_Elements : Storage_Elements.Storage_Count;
      Alignment : Storage_Elements.Storage_Count;
      Subpool : Subpool_Handle)
   is
      pragma Unreferenced (Alignment, Pool);
      Sub : Dynamic_Subpool renames Dynamic_Subpool (Subpool.all);
   begin

      pragma Assert (Is_Owner (Subpool, Current_Task));

      --  If there's not enough space in the current hunk of memory
      if Size_In_Storage_Elements > Sub.Active'Length - Sub.Next_Allocation
      then

         Append (Container => Sub.Used_List, New_Item => Sub.Active);

         if Length (Sub.Free_List) > 0 and then
           Last_Element (Sub.Free_List)'Length >= Size_In_Storage_Elements
         then
            Sub.Active := Last_Element (Sub.Free_List);
            Delete_Last (Sub.Free_List);
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

   procedure Append
     (Container : in out Subpool_Vector;
      New_Item : Dynamic_Subpool_Access) is
   begin
      if Container.Last = Container.Subpool_List.all'Length then
         declare
            Old : Subpool_Array_Access := Container.Subpool_List;
         begin
            Container.Subpool_List := new Subpool_Array (1 .. Old'Length * 2);
            Container.Subpool_List.all (1 .. Old'Length) := Old.all;
            Free_Subpool_List (Old);
         end;
      end if;

      Container.Last := Container.Last + 1;
      Container.Subpool_List (Container.Last) := New_Item;
   end Append;

   --------------------------------------------------------------

   procedure Append (Container : in out Storage_Vector;
                     New_Item : Storage_Array_Access) is
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

   procedure Create_Default_Subpool
     (Pool : in out Dynamic_Pool) is
   begin
      Pool.Default_Subpool := Create_Subpool (Pool'Access);
   end Create_Default_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Pool : access Dynamic_Pool) return Subpool_Handle is
   begin

      if Pool.Default_Block_Size = 0 then
         return Create_Subpool (Pool, Default_Allocation_Block_Size);
      else
         return Create_Subpool (Pool, Pool.Default_Block_Size);
      end if;

   end Create_Subpool;

   --------------------------------------------------------------

   function Create_Subpool
     (Pool : access Dynamic_Pool;
      Block_Size : Storage_Elements.Storage_Count)
      return Subpool_Handle
   is
      New_Subpool : constant Dynamic_Subpool_Access
        := new Dynamic_Subpool (Block_Size => Block_Size);

      Result : constant Subpool_Handle := New_Subpool.all'Unchecked_Access;
   begin

      New_Subpool.Used_List := Storage_Vector'
        (Storage => new Vector (1 .. 1024),
         Last => 0);

      New_Subpool.Free_List := Storage_Vector'
        (Storage => new  Vector (1 .. 1024),
         Last => 0);

      New_Subpool.Active := new System.Storage_Elements.Storage_Array
        (1 .. Block_Size);

      New_Subpool.Next_Allocation := 1;
      New_Subpool.Owner := Ada.Task_Identification.Current_Task;

      Pool.Subpools.Add (New_Subpool);

      Storage_Pools.Subpools.Set_Pool_Of_Subpool
        (Subpool => Result,
         To => Pool.all);

      return Result;

   end Create_Subpool;

   --------------------------------------------------------------

   procedure Deallocate_Subpool
     (Pool : in out Dynamic_Pool;
      Subpool : in out Subpool_Handle)
   is
      The_Subpool : Dynamic_Subpool_Access
        := Dynamic_Subpool (Subpool.all)'Access;

      use type Storage_Pools.Subpools.Subpool_Handle;
   begin

      --  Only removes the access value from the Subpools container
      --  Does not actually delete the object which we still have a
      --  reference to above
      Pool.Subpools.Delete (The_Subpool);

      for I in 1 .. The_Subpool.Used_List.Last loop
         Free_Storage_Array (The_Subpool.Used_List.Storage (I));
      end loop;

      Free_Vector (The_Subpool.Used_List.Storage);

      for I in 1 .. The_Subpool.Free_List.Last loop
         Free_Storage_Array (The_Subpool.Free_List.Storage (I));
      end loop;

      Free_Vector (The_Subpool.Free_List.Storage);

      Free_Storage_Array (The_Subpool.Active);

      --  Handle case when deallocating the default pool
      --  Should only occur if client attempts to obtain the default
      --  subpool, then calls Unchecked_Deallocate_Subpool on that object
      if Pool.Default_Subpool /= null and then Subpool = Pool.Default_Subpool
      then
         Pool.Default_Subpool := null;
      end if;

      Free_Subpool (The_Subpool);

   end Deallocate_Subpool;

   --------------------------------------------------------------

   function Default_Subpool_For_Pool
     (Pool : access Dynamic_Pool)
      return Subpool_Handle is
   begin
      return Pool.Default_Subpool;
   end Default_Subpool_For_Pool;

   --------------------------------------------------------------

   procedure Delete_Last (Container : in out Storage_Vector) is
   begin
      Container.Last := Container.Last - 1;
   end Delete_Last;

   --------------------------------------------------------------

   procedure Finalize   (Pool : in out Dynamic_Pool) is
   begin
      Pool.Subpools.Deallocate_All;
   end Finalize;

   --------------------------------------------------------------

   procedure Finalize (Subpool : in out Scoped_Subpool) is
   begin
      --  Since Ada.Unchecked_Deallocate_Subpool doesn't exist in Ada 2005,
      --  dispatch to Deallocate_Subpool directly.
      Deallocate_Subpool
        (Dynamic_Pool (Storage_Pools.Subpools.Pool_Of_Subpool
         (Subpool.Handle).all),
         Subpool.Handle);
   end Finalize;

   --------------------------------------------------------------

   function Handle
     (Subpool : Scoped_Subpool) return Subpool_Handle is
   begin
      return Subpool.Handle;
   end Handle;

   --------------------------------------------------------------

   function Has_Default_Subpool
     (Pool : Dynamic_Pool) return Boolean
   is
      use type Subpool_Handle;
   begin
      return (Pool.Default_Subpool /= null);
   end Has_Default_Subpool;

   --------------------------------------------------------------

   procedure Initialize (Subpool : in out Scoped_Subpool) is
   begin
      Subpool.Handle := Create_Subpool (Subpool.Pool);
   end Initialize;

   --------------------------------------------------------------

   procedure Initialize (Pool : in out Dynamic_Pool) is
   begin
      Pool.Subpools.Initialize;

      if Pool.Default_Block_Size > 0 then
         Pool.Default_Subpool := Create_Subpool (Pool'Access);
      else
         Pool.Default_Subpool := null;
      end if;

      Pool.Owner := Ada.Task_Identification.Current_Task;
   end Initialize;

   --------------------------------------------------------------

   function Is_Owner
     (Pool : Dynamic_Pool;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Pool.Owner = T);
   end Is_Owner;

   --------------------------------------------------------------

   function Is_Owner
     (Subpool : Subpool_Handle;
      T : Task_Id := Current_Task) return Boolean is
   begin
      return (Dynamic_Subpool (Subpool.all).Owner = T);
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
     (Pool : in out Dynamic_Pool;
      T : Task_Id := Current_Task) is
   begin
      pragma Assert
        ((Is_Owner (Pool, Null_Task_Id) and then T = Current_Task)
         or else (Is_Owner (Pool) and then T = Null_Task_Id));

      Pool.Owner := T;

   end Set_Owner;

   --------------------------------------------------------------

   procedure Set_Owner
     (Subpool : Subpool_Handle;
      T : Task_Id := Current_Task) is
   begin

      pragma Assert
        ((Is_Owner (Subpool, Null_Task_Id) and then T = Current_Task)
         or else (Is_Owner (Subpool) and then T = Null_Task_Id));

      Dynamic_Subpool (Subpool.all).Owner := T;

   end Set_Owner;

   --------------------------------------------------------------

   function Storage_Size
     (Subpool : Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;

   begin
      for I in 1 .. Subpool.Used_List.Last loop
         Result := Result + Subpool.Used_List.Storage (I)'Length;
      end loop;
      for I in 1 .. Subpool.Free_List.Last loop
         Result := Result + Subpool.Free_List.Storage (I)'Length;
      end loop;

      return Result + Subpool.Active'Length;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Size
     (Subpool : Subpool_Handle) return Storage_Elements.Storage_Count
   is
      The_Subpool : constant Dynamic_Subpool_Access :=
        Dynamic_Subpool (Subpool.all)'Access;
   begin
      return Storage_Size (The_Subpool);
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Size
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count is
   begin
      return Pool.Subpools.Storage_Total;
   end Storage_Size;

   --------------------------------------------------------------

   function Storage_Used
     (Subpool : Dynamic_Subpool_Access)
      return Storage_Elements.Storage_Count
   is
      Result : Storage_Elements.Storage_Count := 0;

   begin
      for I in 1 .. Subpool.Used_List.Last loop
         Result := Result + Subpool.Used_List.Storage (I)'Length;
      end loop;

      return Result + Subpool.Next_Allocation - 1;
   end Storage_Used;

   --------------------------------------------------------------

   function Storage_Used
     (Subpool : Subpool_Handle) return Storage_Elements.Storage_Count
   is
      The_Subpool : constant Dynamic_Subpool_Access :=
        Dynamic_Subpool (Subpool.all)'Access;
   begin
      return Storage_Used (The_Subpool);
   end Storage_Used;

   --------------------------------------------------------------

   function Storage_Used
     (Pool : Dynamic_Pool) return Storage_Elements.Storage_Count is
   begin
      return Pool.Subpools.Storage_Usage;
   end Storage_Used;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Subpool
     (Subpool : in out Subpool_Handle)
   is
      use type Storage_Pools.Subpools.Subpool_Handle;
   begin
      if Subpool = null then
         return;
      end if;

      --  Since Ada.Unchecked_Deallocate_Subpool doesn't exist in Ada 2005,
      --  dispatch to Deallocate_Subpool directly.
      Deallocate_Subpool
         (Dynamic_Pool (Storage_Pools.Subpools.Pool_Of_Subpool (Subpool).all),
          Subpool);

   end Unchecked_Deallocate_Subpool;

   package body Subpool_Allocators is

      function Allocate
        (Subpool : Subpool_Handle;
         Value : Allocation_Type := Default_Value)
      return Allocation_Type_Access
      is
         package Subpool_Handle_Conversions is new
           Address_To_Access_Conversions (Object => Allocation_Type);

         Location : System.Address;
      begin

         Allocate_From_Subpool
           (Dynamic_Pool
              (Storage_Pools.Subpools.Pool_Of_Subpool (Subpool).all),
            Storage_Address => Location,
            Size_In_Storage_Elements =>
              Value'Size / System.Storage_Elements.Storage_Element'Size,
            Alignment       => Allocation_Type'Alignment,
            Subpool         => Subpool);

         declare
            Result : constant Allocation_Type_Access :=
              Allocation_Type_Access'
                (Subpool_Handle_Conversions.To_Pointer
                   (Location).all'Unchecked_Access);
         begin
            Result.all := Value;
            return Result;
         end;

      end Allocate;

      --------------------------------------------------------------

      function Allocate
        (Subpool : Scoped_Subpool;
         Value   : Allocation_Type := Default_Value)
      return Allocation_Type_Access
      is
         package Subpool_Handle_Conversions is new
           Address_To_Access_Conversions (Object => Allocation_Type);

         Location : System.Address;
      begin

         Allocate_From_Subpool
           (Dynamic_Pool
              (Storage_Pools.Subpools.Pool_Of_Subpool (Subpool.Handle).all),
            Storage_Address => Location,
            Size_In_Storage_Elements =>
              Value'Size / System.Storage_Elements.Storage_Element'Size,
            Alignment       => Allocation_Type'Alignment,
            Subpool         => Subpool.Handle);

         declare
            Result : constant Allocation_Type_Access :=
              Allocation_Type_Access'
                (Subpool_Handle_Conversions.To_Pointer
                   (Location).all'Unchecked_Access);
         begin
            Result.all := Value;
            return Result;
         end;

      end Allocate;

   end Subpool_Allocators;

end Dynamic_Pools;
