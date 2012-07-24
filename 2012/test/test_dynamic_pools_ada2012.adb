pragma Restrictions
  (No_Implementation_Aspect_Specifications,
   No_Implementation_Attributes,
   No_Implementation_Identifiers,
   No_Implementation_Units);

with Dynamic_Pools; use Dynamic_Pools;
with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements; use System;
with Ada.Finalization;

procedure Test_Dynamic_Pools_Ada2012
is

   Pool : Dynamic_Pools.Dynamic_Pool
     (Default_Block_Size => Dynamic_Pools.Default_Allocation_Block_Size);
   pragma Compile_Time_Warning
     (True, "GNAT Compiler bug, should be able to use default discriminatns");

   pragma Default_Storage_Pool (Pool);

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

   function Recurse (Depth : Natural) return Node_Access
   is
      Sub_Pool : constant Dynamic_Pools.Subpool_Handle
        := Dynamic_Pools.Create_Subpool (Pool);

      Node : constant Node_Access := new (Sub_Pool) Node_Type;

      Name : constant String_Access :=
        new String'("Depth=" & Natural'Image (Depth));

      Description : constant Id_String_Access
      := new (Sub_Pool) String'("ABCDEFGHIJ");

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

   procedure Deallocate_Default_Subpool
   is
      Default_Subpool : Dynamic_Pools.Subpool_Handle :=
        Pool.Default_Subpool_For_Pool;
   begin

      Put_Line ("Deallocating Default Subpool");

      pragma Warnings (Off, "*Default_Subpool*modified*but*never referenced*");

      Dynamic_Pools.Unchecked_Deallocate_Subpool (Default_Subpool);

      pragma Warnings (On, "*Default_Subpool*modified*but*never referenced*");

      Put_Line ("Storage Used=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used) &
                  ", Storage Size=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Size));
   end Deallocate_Default_Subpool;

   List : Node_Access;

   type RC_Access is access Reference_Counted_Type;
   for RC_Access'Storage_Pool use Pool;

   type O_Access is access Ordinary_Type;
   for O_Access'Storage_Pool use Pool;

   Recursion_Depth : constant := 10;

   use type System.Storage_Elements.Storage_Offset;

begin

   New_Line;
   Put_Line ("Initial Storage Used=" &
               Storage_Elements.Storage_Count'Image (Pool.Storage_Used) &
               ", Storage Size=" &
               Storage_Elements.Storage_Count'Image (Pool.Storage_Size));

   Put_Line ("Allocating List Recursively to" &
               Natural'Image (Recursion_Depth) & " subpools");
   Put_Line ("Note: List Nodes are node descriptions allocated to new");
   Put_Line ("       subpools, however, the node names in each node are");
   Put_Line ("       allocated to the default subpool");

   List := Recurse (Recursion_Depth);

   Put_Line ("Storage Used=" &
               Storage_Elements.Storage_Count'Image (Pool.Storage_Used) &
               ", Storage Size=" &
               Storage_Elements.Storage_Count'Image (Pool.Storage_Size));

   Put_Line ("Storage Used in Default Subpool=" &
               Storage_Elements.Storage_Count'Image
       (Dynamic_Pools.Storage_Used
          (Subpool => Pool.Default_Subpool_For_Pool)) &
               ", Storage Size=" &
       Storage_Elements.Storage_Count'Image
       (Dynamic_Pools.Storage_Size
          (Subpool => Pool.Default_Subpool_For_Pool)));

   Put_Line
     ("Bytes Stored in Other subpools=" &
        Storage_Elements.Storage_Count'Image
        (Pool.Storage_Used - Dynamic_Pools.Storage_Used
           (Subpool => Pool.Default_Subpool_For_Pool)));

   pragma Warnings (Off, "*Object*assigned but never read*");

   declare
      Sub_Pool : Dynamic_Pools.Subpool_Handle
        := Dynamic_Pools.Create_Subpool (Pool);

      Object : RC_Access;
   begin

      Put_Line ("Allocating controlled objects needing finalization " &
                  "to a new subpool");

      for I in 1 .. 10 loop
         Object := new (Sub_Pool)
           Reference_Counted_Type;
      end loop;

      Put_Line ("Object Count=" & Natural'Image (Object_Count));
      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image
                  (Pool.Storage_Used));

      Put_Line ("Deallocating Subpool...");

      Dynamic_Pools.Unchecked_Deallocate_Subpool (Sub_Pool);

      Put_Line ("Object Count=" & Natural'Image (Object_Count));
      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));
   end;

   declare

      pragma Suppress (Accessibility_Check);

      Sub_Pool : Dynamic_Pools.Scoped_Subpool
        := Dynamic_Pools.Create_Subpool (Pool => Pool);
      Handle : constant Subpool_Handle := Sub_Pool.Handle;

      pragma Unsuppress (Accessibility_Check);

      Object : RC_Access;
   begin

      Put_Line ("Allocating controlled objects needing finalization " &
                  "to a subpool declared on the stack");

      for I in 1 .. 10 loop
         Object := new (Handle) Reference_Counted_Type;
         pragma Compile_Time_Warning
           (True,
            "GNAT subpool bug, should be able to specify Sub_Pool.Handle");
      end loop;

      Put_Line ("Object Count=" & Natural'Image (Object_Count));
      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

   end;

   pragma Warnings (On, "*Object*assigned but never read*");

   Put_Line ("After Finalization, Object Count=" &
               Natural'Image (Object_Count));
   Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

   begin

      declare
         Sub_Pool : Dynamic_Pools.Subpool_Handle
           := Dynamic_Pools.Create_Subpool (Pool);
      begin

         for I in 1 .. 10 loop
            declare
               Object : constant O_Access
                 := new (Sub_Pool) Ordinary_Type'(Value => I);
            begin
               Put_Line ("Object Value=" & Natural'Image (Object.Value));
            end;
         end loop;

         Put_Line ("Bytes Stored=" &
                     Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

         Put_Line ("Deallocating Subpool...");

         pragma Warnings (Off, "*Sub_Pool* modified*but*never referenced*");
         Dynamic_Pools.Unchecked_Deallocate_Subpool (Sub_Pool);
         pragma Warnings (On, "*Sub_Pool* modified*but*never referenced*");
      end;

      declare
         pragma Suppress (Accessibility_Check);

         Sub_Pool : Dynamic_Pools.Scoped_Subpool
           := Dynamic_Pools.Create_Subpool (Pool);
         Handle : constant Subpool_Handle := Sub_Pool.Handle;

         pragma Unsuppress (Accessibility_Check);
      begin

         Put_Line ("Allocating objects to a new scoped subpool");

         for I in 1 .. 10 loop
            declare
               Object : constant O_Access
                 := new (Handle) Ordinary_Type'(Value => I);

               pragma Compile_Time_Warning
                 (True,
                  "GNAT subpool bug, " &
                    "should be able to specify Sub_Pool.Handle");

               pragma Unreferenced (Object);
            begin
               null;
            end;
         end loop;

         Put_Line ("Bytes Stored Before Finalization=" &
                     Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

      end;

      Put_Line ("Bytes Stored After Finalization=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));
   end;

   Print (List.all);

   Deallocate_Default_Subpool;

   Put_Line
     ("Bytes Stored in Default Subpool=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Used
           (Subpool => Pool.Default_Subpool_For_Pool)));

   pragma Warnings (Off, "*Object*is assigned but never read*");
   declare
      Object : RC_Access;
   begin
      Put_Line ("Allocating some more objects needing finalization " &
                  "to the default subpool");

      for I in 1 .. 10 loop
         Object := new Reference_Counted_Type;
      end loop;

      Put_Line ("Object Count=" & Natural'Image (Object_Count));
      Put_Line ("Bytes Stored=" &
                  Storage_Elements.Storage_Count'Image (Pool.Storage_Used));

      Put_Line
        ("Bytes Stored in Default Subpool=" &
           Storage_Elements.Storage_Count'Image
           (Dynamic_Pools.Storage_Used
              (Subpool => Pool.Default_Subpool_For_Pool)));
   end;
   pragma Warnings (On, "*Object*is assigned but never read*");

   Put_Line ("Deallocating Default Subpool again");

   Deallocate_Default_Subpool;

   Put_Line
     ("Bytes Stored in Default Subpool=" &
        Storage_Elements.Storage_Count'Image
        (Dynamic_Pools.Storage_Used
           (Subpool => Pool.Default_Subpool_For_Pool)));

   Put_Line ("At this point, the nodes and their descriptions still exist,");
   Put_Line ("because their subpools still exist, however the node names");
   Put_Line ("shouldn't exist, because the default subpool had been freed.");
   Put_Line ("In GNAT this still may print out, probably because the memory");
   Put_Line ("has not been overwritten, but this is dangerous");
   Put_Line ("Don't be surprised if you see a storage error exception");

   begin
      Print (List.all);
   exception
      when Storage_Error =>
         New_Line;
         Put_Line ("STORAGE_ERROR was raised, which is not surprising");
   end;

   New_Line;
   Put_Line ("Successful Completion");
   New_Line;

end Test_Dynamic_Pools_Ada2012;
