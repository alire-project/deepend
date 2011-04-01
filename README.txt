1.0 INTRODUCTION
================

Deepend is a storage pool with Subpool capabilities for Ada 2005 where all the
objects in a subpool can be reclaimed all at once, instead of requiring each
object to be individually reclaimed one at a time. Subpools may be completely
independent pools, or they may be chained together in order to extend the 
lifetime of child pools to that of the lifetime of the root parent pool.

Rather than deallocate items individually which is error prone and subceptable
to memory leaks and other memory issues, a subpool can be freed all at once 
automatically when the pool object goes out of scope. 

In addition, the pool may be reclaimed multiple times before the end of its
lifetime through an explicit call to Unchecked_Deallocate.

Have you ever wondered why Deallocating in Ada is called Unchecked_Deallocation,
and requires a bit more effort than delete in other languages?

Part of the reason has to do with the potential memory issues alluded to above.

With this this Storage pool, Unchecked_Deallocation is implemented as a No OP 
(null procedure), because it is not needed or intended to be used.

A subpool based storage management provides a safer means of memory management,
which can outperform other mechanisms for storage reclamation including 
garbage collection.

Deepend is free software.  See the file COPYING for copying permission. Most of 
the source code is released under the GMGPL (GNAT Modified GPL) See the 
individual source files for details.

Any comments on the generics would be greatly appreciated.
Please send comments to brad.moore@shaw.ca

2. LIMITATIONS
==============
It is erroneous to allocate objects that need finalization
  eg. (Tasks, or objects of types inherited from types defined in
       Ada.Finalization) from this storage pool
and then Release the storage associated with those objects before they would 
have otherwise been finalized. (Either through a call to 
Unchecked_Deallocate_Storage, or through a call to Unchecked_Deallocate_Objecs
for a parent pool that has a subpool containing such objects

It is OK however to finalize a dynamic pool containing such objects if the
access types for these objects are finalized before the pool is finalized,
since Ada performs finalization of all allocated objects needing finalization
from an access type when an access type is finalized.

Access types for indefinite objects, such as variable length arrays and 
variable sized record types with discriminants, i,e, fat pointers, cannot be
allocated using the generic Allocate or Initialized_Allocate procedures.
They may be allocated however using Ada's new operator, except this precludes
the ability to dynamically specify the pool object. It uses the pool object
that is statically associated with the access type.

It is currently proposed that Ada 2012 will contain mechanisms to free such 
objects needing finalization from a storage pool prior to the finalization of
the pool, as well as provide syntax to allow fat pointers to be allocated to
a subpool.

Upgrading this package to Ada 2012 may (and likely will) involve making 
changes to the visible specifications of this package, though existing 
interfaces may be kept for backwards compatibility.

3. DOWNLOADING
==============
The latest stable release and older releases may be downloaded from;

https://sourceforge.net/projects/deepend/files/

For those who want the current development versions of the source they
can download using git (http://git-scm.com/) by issuing the following commands

  mkdir sandbox
  cd sandbox
  git clone git://deepend.git.sourceforge.net/gitroot/deepend/deepend

The current development version typically will correspond to the latest stable
release, but may at times be unstable when new features are being worked on.

2. SUPPORTED TARGETS AND COMPILERS
=================================

The storage pool has been tested on Linux and Windows.

3. TEST EXECUTABLES
===================

A simple test executable test_deepend executable exercises the bindings.

The bindings have also been tested with Paraffin (See 
http://paraffin.sourceforge.net) code to create a binary tree in parallel to a 
predetermined depth. This excercised the subpool capability, as each worker task
created its own subpool to store its own nodes, which were later processed by 
the parent task.

4. WHY DEEPEND?
===============
  1) A pool has to be pretty deep if it is to have subs floating in it.
  2) Hopefully it can be used to write deependable software.
  3) Hopefully it doesn't mean this is something the author has gone off of.

Brad Moore