--  Dummy package intended to represent the standard System package
--  This makes it easier to make it appear that a child of System is being
--  used, since you cant declare your own child packages
--  under System. Once Ada 2012 is available, this package will be deleted.
package Sys is
   pragma Pure;
end Sys;
