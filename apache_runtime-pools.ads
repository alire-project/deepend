--                                                                          --
--    Deepend - Mark and Release Storage Pool for Ada 2005 with Subpools    --
--                                                                          --
--                  A P A C H E _ R U N T I M E  (Bindings)                 --
--                               P O O L S                                  --
--                                                                          --
--                                S p e c                                   --
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

with System;
with Interfaces.C;
use Interfaces;

package Apache_Runtime.Pools is

   subtype Pool_Type is System.Address;
   subtype Pool_Access is System.Address;

   function Initialize return Apr_Status;

   function Create
     (New_Pool : Pool_Access;
      Parent : Pool_Type) return Apr_Status;

   procedure Destroy (Pool : Pool_Type);

   function Allocate (Pool : Pool_Type; Size : Apr_Size) return System.Address;

   procedure Clear (Pool : Pool_Type);

   function Is_Ancestor (A, B : Pool_Type) return Boolean;
   function Get_Parent (Child : Pool_Type) return Pool_Type;

   function Set_User_Data
     (Data : System.Address;
      Key : C.char_array;
      Cleanup : access function return Apr_Status;
      Pool : Pool_Type) return Apr_Status;

   function Get_User_Data
     (Data : System.Address;
      Key : C.char_array;
      Pool : Pool_Type) return Apr_Status;

private

   pragma Import (C, Initialize, "apr_initialize");
   pragma Import (C, Destroy, "apr_pool_destroy");
   pragma Import (C, Allocate, "apr_palloc");
   pragma Import (C, Clear, "apr_pool_clear");
   pragma Import (C, Get_Parent, "apr_pool_parent_get");
   pragma Import (C, Set_User_Data, "apr_pool_userdata_setn");
   pragma Import (C, Get_User_Data, "apr_pool_userdata_get");
   pragma Inline (Is_Ancestor, Create);

end Apache_Runtime.Pools;
