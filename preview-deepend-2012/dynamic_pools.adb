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

package body Dynamic_Pools is

   function Objects_Need_Finalization
     (Pool : Dynamic_Pool'Class) return Boolean is
      --  See Unchecked_Deallocate_Objects. Since we cannot know this, and
      --  cant support allocation of objects needing finalization we
      --  always return false.
      pragma Unreferenced (Pool);
   begin
      return False;
   end Objects_Need_Finalization;

   --------------------------------------------------------------

   procedure Unchecked_Deallocate_Objects
     (Pool : in out Dynamic_Pool) is
   --  Currently only the vendor could supply such a routine. Ada 2012
   --  will expose this functionality, but for now we have to say that
   --  it is erroneous to allocate objects needing finalization such as tasks,
   --  protected objects and type derived types defined in Ada.Finalization
   --  from a dynamic pool.
   begin
      null;
   end Unchecked_Deallocate_Objects;

end Dynamic_Pools;
