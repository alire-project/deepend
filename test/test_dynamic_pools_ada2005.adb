with Dynamic_Pools; use Dynamic_Pools;
with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements; use System;

procedure Test_Dynamic_Pools_Ada2005
is
   Pool : aliased Dynamic_Pools.Dynamic_Pool
     (Default_Block_Size => Dynamic_Pools.Default_Allocation_Block_Size);

   subtype Id_String is String (1 .. 10);
   type Id_String_Access is access Id_String;
   for Id_String_Access'Storage_Pool use Pool;

   type String_Access is access String;
   for String_Access'Storage_Pool use Pool;

   type Node_Type is record
      Value : Integer;
      Name : access String;
      Description : Id_String_Access;
      Next : access Node_Type;
   end record;

   type Node_Access is access Node_Type;
   for Node_Access'Storage_Pool use Pool;

   function New_Node is new Dynamic_Pools.Allocation
     (Node_Type,
      Node_Access);

   type Ordinary_Type is
      record
         Value : Integer;
      end record;

   type O_Access is access Ordinary_Type;
   for O_Access'Storage_Pool use Pool;

   function New_Ordinary_Type is new Dynamic_Pools.Allocation
     (Ordinary_Type,
      O_Access);

   function New_String is new Dynamic_Pools.Initialized_Allocation
     (Allocation_Type => Id_String,
      Allocation_Type_Access => Id_String_Access);

   function Recurse (Depth : Natural) return Node_Access
   is
      Sub_Pool : constant Dynamic_Pools.Subpool_Handle
        := Dynamic_Pools.Create_Subpool (Pool'Access);

      Node : constant Node_Access := New_Node (Sub_Pool);

      Name : constant String_Access :=
        new String'("Depth=" & Natural'Image (Depth));

      Description : constant Id_String_Access
        := New_String (Subpool => Sub_Pool,
                       Qualified_Expression => "ABCDEFGHIJ");

   begin
      if Depth = 0 then
         Node.all := (Value => 0,
                      Name => Name.all'Unchecked_Access,
                      Description => Description,
                      Next => null);
         return  Node;
      else
         Node.all := (Value => Depth,
                      Name => Name.all'Unchecked_Access,
                      Description => Description,
                      Next => Recurse (Depth - 1));
         return Node;
      end if;
   end Recurse;

   procedure Print (List : Node_Type)
   is
   begin
      if List.Next /= null then
         Print (List.Next.all);
      end if;

      Put_Line (Integer'Image (List.Value) &
                ", Name=<" & List.Name.all &
                ">, Desc=<" & List.Description.all & '>');
   end Print;

   List : constant Node_Access := Recurse (10);

begin

   begin

      declare
         Sub_Pool : Dynamic_Pools.Subpool_Handle
           := Dynamic_Pools.Create_Subpool (Pool'Access);
      begin

         for I in 1 .. 10 loop
            declare
               Object : constant O_Access
                 := New_Ordinary_Type (Sub_Pool);
            begin
               Put_Line ("Object Value=" & Natural'Image (Object.Value));
            end;
         end loop;

         Put_Line ("Bytes Stored=" &
                     Storage_Elements.Storage_Count'Image (Pool.Storage_Size));

         Put_Line ("Deallocating Subpool...");

         pragma Warnings (Off, "*Sub_Pool* modified*but*never referenced*");
         Dynamic_Pools.Unchecked_Deallocate_Subpool (Sub_Pool);
         pragma Warnings (On, "*Sub_Pool* modified*but*never referenced*");
      end;

      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Size));
   end;

   Print (List.all);
end Test_Dynamic_Pools_Ada2005;
