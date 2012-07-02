with Dynamic_Pools; use Dynamic_Pools;
with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements; use System;
with Ada.Finalization;
procedure Test_Dynamic_Pools_Ada2012
is
   Pool : Dynamic_Pools.Dynamic_Pool
     (Default_Block_Size => Dynamic_Pools.Default_Allocation_Block_Size
     );

   type Ordinary_Type is
      record
         Value : Integer;
      end record;

   type Reference_Counted_Type is new Ada.Finalization.Controlled with
      record
         Value : Integer;
      end record;

 Object_Count : Natural := 0;

      overriding procedure Initialize
        (Object : in out Reference_Counted_Type);
      overriding procedure Finalize
        (Object : in out Reference_Counted_Type);
      overriding procedure Adjust
        (Object : in out Reference_Counted_Type);

   overriding procedure Initialize (Object : in out Reference_Counted_Type)
   is
   begin
      Object.Value := 0;
      Object_Count := Object_Count + 1;
      Put_Line ("Called Init");
   end Initialize;

   overriding procedure Adjust (Object : in out Reference_Counted_Type)
   is
   begin
      Object.Value := 0;
      Object_Count := Object_Count + 1;
      Put_Line ("Called Adjust");
   end Adjust;

   overriding procedure Finalize   (Object : in out Reference_Counted_Type)
   is
   begin
      Object.Value := -1;
      Object_Count := Object_Count - 1;
      Put_Line ("Called Final");
   end Finalize;

   type RC_Access is access Reference_Counted_Type;
   for RC_Access'Storage_Pool use Pool;

   type O_Access is access Ordinary_Type;
   for O_Access'Storage_Pool use Pool;

begin
   begin

      declare
         Sub_Pool : Dynamic_Pools.Subpool_Handle
           := Dynamic_Pools.Create_Subpool (Pool);
         Object1 : constant RC_Access
           := new (Sub_Pool) Reference_Counted_Type;

        --  '(Ada.Finalization.Controlled with Value => 1);
      begin
         Put_Line ("Object Value=" & Natural'Image (Object1.Value));
         Put_Line ("Object Count=" & Natural'Image (Object_Count));
         Put_Line ("Bytes Stored=" &
                     Storage_Elements.Storage_Count'Image (Pool.Storage_Size));

         pragma Warnings (Off, "*Sub_Pool* modified*but*never referenced*");
         Dynamic_Pools.Unchecked_Deallocate_Subpool (Sub_Pool);
         pragma Warnings (On, "*Sub_Pool* modified*but*never referenced*");
      end;

      Put_Line ("Object Count=" & Natural'Image (Object_Count));
      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Size));
   end;

   begin

      declare
         Sub_Pool : Dynamic_Pools.Subpool_Handle
           := Dynamic_Pools.Create_Subpool (Pool);
         Object1 : constant O_Access
           := new (Sub_Pool) Ordinary_Type;

        --  '(Ada.Finalization.Controlled with Value => 1);
      begin
         Put_Line ("Object Value=" & Natural'Image (Object1.Value));
         Put_Line ("Bytes Stored=" &
                     Storage_Elements.Storage_Count'Image (Pool.Storage_Size));

         pragma Warnings (Off, "*Sub_Pool* modified*but*never referenced*");
         Dynamic_Pools.Unchecked_Deallocate_Subpool (Sub_Pool);
         pragma Warnings (On, "*Sub_Pool* modified*but*never referenced*");
      end;

      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Size));
   end;

end Test_Dynamic_Pools_Ada2012;
