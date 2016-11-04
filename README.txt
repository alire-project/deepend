1.0 INTRODUCTION
================

Deepend is a suite of dynamic storage pools with subpool capabilities for
Ada 95, Ada 2005, and Ada 2012. Bounded and unbounded storage pools types
exist. Storage pools with subpool capabilities allow all objects in a subpool
to be reclaimed all at once, instead of requiring each object to be individually
reclaimed one at a time. A Dynamic Pool may have any number of subpools. 
If subpools are not reclaimed prior to finalization of the pool, then they are 
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

2.0 DOWNLOADING
===============

The latest stable release and older releases may be downloaded from;

https://sourceforge.net/projects/deepend/files/

For those who want the current development versions of the source they
can download using git (http://git-scm.com/) by issuing the following
commands

  mkdir sandbox
  cd sandbox
  git clone git://git.code.sf.net/p/deepend/code deepend-src

The current development version typically will correspond to the latest
stable release, but may at times be unstable when new features are being
worked on.

3.0 DEEPEND Storage Pool Classes
================================

There are 4 Storage Pool packages to choose from in Deepend.

     1) Dynamic_Pools
     2) Bounded_Dynamic_Pools
     3) Basic_Dynamic_Pools
     4) Basic_Bounded_Dynamic_Pools

  The Dynamic_Pools package has subpool capabilities where the storage
  in each Subpool object is unbounded. If the current block of memory
  is fully allocated to objects, and further objects are allocated,
  then another block of storage is allocated to the subpool, and further
  allocations to that subpool are carved out of that new storage block.

  The Bounded_Dynamic_Pools package has subpool capabilities where the
  storage in each Subpool object is bounded. If the Subpool is fully
  allocated with objects, and an attempt is made to allocate further objects
  from the same subpool, then a Storage_Error exception is raised.

  The Basic_Dynamic_Pool package does not have subpool capabilities, and
  each allocation is managed instead by the pool object. When the pool is
  finalized, all objects allocated from the pool are also finalized.
  A Basic_Dynamic_Pool can be thought of as a Dynamic_Pool with a single 
  subpool. A Basic_Dynamic_Pool is an unbounded pool such that if the current
  block of storage is fully allocated with objects, and further objects are
  allocated, then another block of memory is allocated to the pool, and
  further object allocations are carved out of that new block,

  The Basic_Bounded_Dynamic_Pool package does not have subpool capabilities,
  and each allocation is managed instead by the pool object. Like the
  Basic_Dynamic_Pool, when the pool is finalized, all objects allocated from
  the pool are also finalized. A Basic_Dynamic_Pool is a bounded pool such that
  if the pool's storage has been fully allocated with objects, and an attempt
  is made to allocate further objects, then a Storage_Error exception is raised.

  Both Basic_Dynamic_Pools and Bounded_Basic_Dynamic_Pools are forward 
  compatible with the Ada 2012 standard for Storage_Pools, since they only 
  allows allocations via the existing "new" operator (without subpool 
  specifications). This facility relies on access type finalization to free all
  the objects from a pool, and does not otherwise support subpools.

  In Ada 2012, the new allocation syntax may also be used with Dynamic_Pools
  and Bounded_Dynamic_Pools in order to specify the subpool that will contain
  the allocated objects.

   e.g.
        Object := new (subpool_name) Object_Type'(Value);

  For Ada 95 and Ada 2005, a similar effect can be obtained by using the
  Allocation and Initialized_Allocation generics provided by the packages.
  However, these generics only allow allocating non-controlled objects of
  definite types to a particular subpool, whereas in Ada 2012, indefinate
  types and controlled types, and other types needing finalization such as
  protected types may also be allocated to a subpool. Only task types or type
  that have tasks cannot be allocated to a subpool in Ada 2012.

  In addition, for Ada 95, Ada 2005, and Ada 2012, the "new" keyword may be
  used with all the subpool packages without specifying a subpool, which results
  in an object being allocated to the default subpool for the storage pool.

  The Dynamic_Pool and Bounded_Dynamic_Pool allow the default subpool to be
  deallocated. Another default subpool can be reinstated by calling the
  Create_Default_Subpool subprogram.

4.0 BUILD INSTRUCTIONS 
======================

- For the Irvine ICC Ada 2005 compiler on  Windows, execute the 
  following script to create the Ada 2005 versions of the executables;

   cd 2005
   icm new
   icm scan -subdir "*.ad?"
   icm make test_dynamic_pools_ada2005
   icm make test_bounded_dynamic_pools_ada2005
   icm make binary_trees_without_subpools_ada2005
   icm make bounded_binary_trees_without_subpools_ada2005
   icm make binary_trees_with_subpools_ada2005
   icm make bounded_binary_trees_with_subpools_ada2005

- For the Irvine ICC Ada 2005 compiler on  Windows, execute the 
  following script to create the Ada 95 versions of the executables;

   cd 95
   icm new
   icm scan -subdir "*.ad?"
   icm make test_dynamic_pools_ada95
   icm make test_bounded_dynamic_pools_ada95
   icm make binary_trees_without_subpools_ada95
   icm make bounded_binary_trees_without_subpools_ada95
   icm make binary_trees_with_subpools_ada95
   icm make bounded_binary_trees_wtih_subpools_ada95

  You can add other compile flags as well, such as
      -compile_flags=\"-predef=(f32,lf64) -opt -debug -nochecks\"    

  to turn on optimization, debug, and disable checks.
 
  To compile for Irvine ICC on Linux, the script is the same, except
  that if compile options are used then the options should be enclosed
  with single quotes, and \" should be replaced with '"'.

  i.e.
      -compile_flags='"-predef=(f32,lf64) -opt -debug -nochecks"'    

- For GNAT Pro, GNAT GPL or GNAT AUX, load the appropriate .gpr file 
  from either the 95, 2005, or 2012 subfolder into the GPS ide, and build
  the executable from within the ide, or alternatively use gnatmake to 
  perform the equivalent actions described in the .gpr file.
  You can also execute the master build for all projects by entering
  the following command from the command line from the root folder.
 
       gprbuild make_all.gpr


5.0 TESTED PLATFORMS
====================

Deepend has been ported to the following compilers and platforms.

   GNAT GPL 2010-2016  (Windows, Linux)  Ada95, Ada2005, Ada2012
   Irvine Ada 2005     (Windows, Linux)  Ada95, Ada2005
   GNAT AUX FSF 4.6.1-4.9.4  (Android, Rasperry Pi)  Ada95, Ada2005, Ada2012

Deepend is intended to be portable to any platform that supports 
Ada95, Ada 2005 or Ada 2012 compilation, and in theory, any Ada95, 
Ada 2005,  or Ada 2012 compiler should be able to compile the code 
since there are no dependencies on vendor specific run-time libraries.

It should also be possible to compile Deepend for any target
system, since Deepend does not rely on any OS-specific support.

6.0 LIMITATIONS
===============

For the Ada 95, and Ada 2005 versions of the packages, it is erroneous
to allocate objects of unconstrained types and objects that need 
finalization.
  eg. (Tasks, protected objects, or objects of types inherited from 
       types defined in Ada.Finalization) from this storage pool
and then Deallocate the subpool associated with those objects before 
the objects would have otherwise been finalized. 

For the Ada 2012 version of the packages, it is only erroneous to
allocate task objects, or objects that contain tasks to a subpool.
Objects of unconstrained types, protected types, and controlled types
may be allocated to a subpool, and will be properly deallocated if
the containing subpool is deallocated.

The main differences between the Ada 95 version and the Ada 2005 version
is that the Ada 2005 version uses newer language features including overriding
clauses, null procedures, private withs, not null access parameters, 
object prefix notation, default box initializers, and the Ada 2005 container 
libraries.

The main differences between the Ada 2005 version and the Ada 2012
version of the Dynamic_Pools package is that the Ada 2012 version
takes advantages of the new features of the language, including
defaults for discriminated types, functions with in out parameters 
instead of access parameters, pre/post conditions, expression functions,
subtype predicates, type invariants, simpler iterator syntax, and most 
importantly utilization of the new standard subpools storage package, 
Ada.Storage_Pools.Subpools. In Ada 2012, the Default_Storage_Pool 
pragma may be used to specify a Deepend pool as the default storage
pool. See RM (13.11.3).

7.0 TEST EXECUTABLES
====================

A simple test executable test_dynamic_pools executable exercises the 
pool. There are Ada 95, Ada 2005, and Ada 2012 versions of this test driver.

In addition, there are binary_trees test executables with two different
implementations of a benchmark test, for Ada 95,  Ada 2005 and Ada 2012.
    - the implementations (bounded and unbounded) under the folder;
        binary_tree_benchmark_using_access_types_with_dedicated_pools
      performs all allocations using the new operator and relies on
      Ada's access type finalization to release all objects from the
      subpool. This test utilizes the Basic_Dynamic_Pools package,
      and has been found to give the best test results for this
      benchmark in Ada 95, Ada 2005 and Ada 2012 
    - the implementations (bounded and unbounded) under the folder;
       binary_tree_benchmark_using_subpool_deallocations performs all
       allocations using the Ada 2012 subpool allocator syntax,
       or Deepend's Allocate generic for Ada 2005, rather than using
       the tradional "new" operator with Ada 83 syntax.

8.0 WHY DEEPEND?
================
  1) Its the end of the pool you'd expect to go to, if the pool is to 
     have subs floating in it.
  2) Hopefully it can be used to write deependable software.
  3) Hopefully it doesn't mean this is something the author has gone off of.

Brad Moore
