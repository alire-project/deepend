--                                                                          --
--    Deepend - Mark and Release Storage Pool for Ada 2005 with Subpools    --
--                                                                          --
--                  A P A C H E _ R U N T I M E  (Bindings)                 --
--                               P O O L S                                  --
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

package body Apache_Runtime.Pools is

   function Create_Ex
     (New_Pool : Pool_Access;
      Parent : Pool_Type;
      Reserved_1, Reserved_2 : System.Address) return Apr_Status;
   pragma Import (C, Create_Ex, "apr_pool_create_ex");

   function APR_Is_Ancestor
     (A, B : Pool_Type) return C.int;
   pragma Import (C, APR_Is_Ancestor, "apr_pool_is_ancestor");

   ------------
   -- Create --
   ------------

   function Create
     (New_Pool : Pool_Access;
      Parent : Pool_Type)
      return Apr_Status
   is
   begin
      return Create_Ex
        (New_Pool,
         Parent,
         System.Null_Address,
         System.Null_Address);
   end Create;

   function Is_Ancestor (A, B : Pool_Type) return Boolean is
      use type C.int;
   begin
      return (APR_Is_Ancestor (A, B) /= 0);
   end Is_Ancestor;

end Apache_Runtime.Pools;
