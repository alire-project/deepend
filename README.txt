1.0 INTRODUCTION
================

Deepend is a dynamic storage pool with Subpool capabilities for Ada 2005
and Ada 2012 where all the objects in a subpool can be reclaimed all at
once, instead of requiring each object to be individually reclaimed one 
at a time. A Dynamic Pool may have any number of subpools. If subpools 
are not reclaimed prior to finalization of the pool, then they are 
finalized when the pool is finalized.

Rather than deallocate items individually which is error prone and
subceptable to memory leaks and other memory issues, a subpool can be 
freed all at once automatically when the pool object goes out of scope. 

Have you ever wondered why Deallocating in Ada is called 
Unchecked_Deallocation, or why Ada has a new operator, but not a delete
operator?

Part of the reason is that Ada was designed with safety in mind, and 
heap deallocations are viewed as error prone as it is in many languages. 

With this Storage pool, Unchecked_Deallocation is implemented as a No-Op 
(null procedure), because it is not needed or intended to be used.

Subpool based storage management provides a safer means of memory
management, which can outperform other mechanisms for storage 
reclamation including garbage collection.

Deepend is free software.  See the file COPYING for copying permission.
Most of the source code is released under the GMGPL (GNAT Modified GPL).
See the individual source files for details.

Any comments on the generics would be greatly appreciated.
Please send comments to brad.moore@shaw.ca

2.0 BUILD INSTRUCTIONS =========

- For the Irvine ICC Ada 2005 compiler on  Windows, execute the 
  following script;

   icm new
   icm scan -subdir "*.ad?"
   icm scan 2005/*.ad{s,b}
   icm scan test/binary_tree_benchmark_using_access_types_with_dedicated_pools/*.ad{s,b}
   REM icm scan test/binary_tree_benchmark_using_subpool_allocations/*.ad{s,b}
   icm make test_dynamic_pools_ada2005
   icm make binary_trees_without_subpools
   REM icm make binary_trees_with_subpools_ada2005



  You can add other compile flags as well, such as
      -compile_flags=\"-predef=(f32,lf64) -opt -debug -nochecks\"    

  to turn on optimization, debug, and disable checks.

  Note that moving the REM above to the beginning of the previous line
  causes the binary_trees executable to use implementation contained in
  the binary_tree_benchmark_using_subpool_deallocations folder.

 
  To compile for Irvine ICC on Linux, the script is the same, except
  that if compile options are used then the options should be enclosed
  with single quotes, and \" should be replaced with '"'.

  i.e.
      -compile_flags='"-predef=(f32,lf64) -opt -debug -nochecks"'    

- For GNAT Pro, GNAT GPL or GNAT AUX, load the appropriate .gpr file 
  into the GPS ide, and build the executable from within the ide, or 
  alternatively use gnatmake to perform the equivalent actions described
  in the .gpr file.

3.0 TESTED PLATFORMS =========

Deepend has been ported to the following compilers and platforms.

   GNAT GPL 2010-2012  (Windows, Linux)
   Irvine Ada 2005     (Windows, Linux)
   GNAT AUX FSF 4.6.1  (Android)

Deepend is intended to be portable to any platform that supports 
Ada 2005 or Ada 2012 compilation, and in theory, any Ada 2005 or Ada
2012 compiler should be able to compile the code since there are no 
dependencies on vendor specific run-time libraries.

It should also be possible to compile Deepend for any target
system, since Deepend does not rely on any OS-specific support.

4.0 LIMITATIONS
==============
For the Ada 2005 version of the packages, it is erroneous to allocate
objects of unconstrained types and objects that need finalization
  eg. (Tasks, protected objects, or objects of types inherited from 
       types defined in Ada.Finalization) from this storage pool
and then Deallocate the subpool associated with those objects before 
the objects would have otherwise been finalized. 

For the Ada 2012 version of the packages, it is only erroneous to
allocate task objects, or objects that contain tasks to a subpool.
Objects of unconstrained types, protected types, and controlled types
may be allocated to a subpool, and will be properly deallocated if
the containing subpool is deallocated.

The main differences between the Ada 2005 version and the Ada 2012
version of the Dynamic_Pools package is that the Ada 2012 version
takes advantages of the new features of the language, including
defaults for discriminated types, functions with in out parameters 
instead of access parameters, pre/post conditions, and utilization of
the new standard subpools storage package, Ada.Storage_Pools.Subpools.

5.0 DOWNLOADING
==============
The latest stable release and older releases may be downloaded from;

https://sourceforge.net/projects/deepend/files/

For those who want the current development versions of the source they
can download using git (http://git-scm.com/) by issuing the following
commands

  mkdir sandbox
  cd sandbox
  git clone git://deepend.git.sourceforge.net/gitroot/deepend/deepend

The current development version typically will correspond to the latest
stable release, but may at times be unstable when new features are being
worked on.

6.0 DEEPEND Storage Pool Classes
================================

Deepend currently provides two options for storage management

  1) Basic_Dynamic_Pools
  2) Dynamic_Pools

Basic_Dynamic_Pools is forward compatible with the Ada 2012 standard
for Storage_Pools, since it only allows allocations via the existing 
"new" operator (without subpool specifications). This facility relies 
on access type finalization to free all the objects from a pool, and
does not otherwise support subpools.

Dynamic_Pools provides the capabilities of Basic_Dynamic_Pools, but in
addition allows the creation of subpools, and allocations can be made
from subpools. Subpools can be deallocated, which deallocates all
objects allocated from the subpool. 

7.0 TEST EXECUTABLES
===================

A simple test executable test_deepend executable exercises the pool.

In addition, there is a binary_trees test executable with two different
implementations of a benchmark test.
    - the implementation under the folder;
        binary_tree_benchmark_using_access_types_with_dedicated_pools
      performs all allocations using the new operator and relies on
      Ada's access type finalization to release all objects from the
      subpool.
    - the implementation under the folder;
       binary_tree_benchmark_using_subpool_deallocations performs all
       allocations using Deepend's Allocate generic, rather than using
       the "new" operator.

8.0 WHY DEEPEND?
===============
  1) Its the end of the pool you'd expect to go to, if the pool is to 
     have subs floating in it.
  2) Hopefully it can be used to write deependable software.
  3) Hopefully it doesn't mean this is something the author has gone off of.

Brad Moore
