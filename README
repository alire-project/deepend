1.0 INTRODUCTION
================

Deepend is a storage pool with Subpool capabilities for Ada 2005 where all the
objects in a subpool can be reclaimed all at once, instead of requiring each
object to be individually reclaimed one at a time. Subpools may be completely
independent pools, or they may be chained together in order to extend the 
lifetime of child pools to that of the lifetime of the root parent pool.

Deepend is a binding to the Apache Runtime Pools implementation.

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


2. SUPPORTED TARGETS AND COMPILERS
=================================

The storage pool has only been tested on Linux. The storage pool bindings 
themselves should be portable to any Ada 2005 supported platform, however since 
the software links to the Apache Runtime, the software is also limited to 
platforms where the Apache Runtime libraries have been ported.

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
