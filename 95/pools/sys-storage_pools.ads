--  Dummy package intended to represent the standard System.Storage_Pools
--  package.
--  This makes it easier to make it appear that a child of System.Storage_Pools
--  is being used, since you cant declare your own child packages
--  under System. When Ada 2012 is available, this package will be deleted.
package Sys.Storage_Pools is
   pragma Preelaborate;
end Sys.Storage_Pools;
