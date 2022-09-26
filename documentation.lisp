#|
 This file is a part of Luckless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

;;; hashtable.lisp
(in-package #:org.shirakumo.luckless.hashtable)
(docs:define-docs
  (function make-castable
            "Create a new castable.

TEST should be one of EQ EQL EQUAL EQUALP
SIZE should be a base size to start the table with. May be modified by
the implementation.
HASH-FUNCTION should be a function of one argument that returns a
integer used as a hash for the argument. Unless explicitly passed,
will determine the hash function to use based on TEST.

See CASTABLE (type)")
  
  (type castable
        "A lock-free hash-table.

It is safe to set entries from any number of threads simultaneously.
Note that the hash table will never shrink, even if it is cleared or
entries are removed from it. If you need to shrink the table, discard
it and create a new one instead.

See MAKE-CASTABLE
See CASTABLE-P
See SIZE
See COUNT
See TEST
See HASH-FUNCTION
See GETHASH
See REMHASH
See TRY-REMHASH
See PUT-IF-ABSENT
See PUT-IF-EQUAL
See PUT-IF-PRESENT
See CLRHASH
See MAPHASH")

  (function castable-p
            "Returns true if the object is a CASTABLE

See CASTABLE (type)")

  (function size
            "Returns the current capacity of the table.

See COUNT")

  (function count
            "Returns the current number of entries in the table.

See SIZE")

  (function test
            "Returns the equality test function.")

  (function hash-function
            "Returns the hashing function used.")

  (function gethash
            "Accesses an entry in the hash table.

If no entry was present, DEFAULT is returned.

IF-EXISTS can be one of the following:
  :OVERWRITE --- Replace the entry's value
  :ERROR     --- Signal an error
  NIL        --- Do nothing
IF-DOES-NOT-EXIST can be one of the following:
  :OVERWRITE --- Set the entry's value
  :ERROR     --- Signal an error
  NIL        --- Do nothing

This is safe to call from any number of threads.")

  (function remhash
            "Removes an entry from the hash table.

Returns T if there was an entry in the table.

This is safe to call from any number of threads.")

  (function try-remhash
            "Removes the entry form the table IFF the value is still the specified one.")

  (function put-if-absent
            "Sets the entry IFF the entry does not exist yet.")

  (function put-if-equal
            "Sets the entry IFF the entry already exists and its current value is the specified old-value.")

  (function put-if-present
            "Sets the entry IFF the entry already exists.")

  (function clrhash
            "Clears the hash table removing all entries.")

  (function maphash
            "Maps over the hash table.

FUNCTION must accept two arguments, the KEY and the VALUE of the entry
currently being mapped."))

;;; list.lisp
(in-package #:org.shirakumo.luckless.list)
(docs:define-docs
  (type caslist
        "A lock-free linked list.

It is safe to add and remove entries from any number of threads
simultaneously.

See CASLIST (function)
See CASLIST-P
See TO-LIST
See MAPC
See FIRST
See NTH
See LENGTH
See PUSH
See DELETE
See MEMBER")

  (function caslist
            "Returns a new CASLIST containing ELEMENTS.

See CASLIST (type)")
  
  (function caslist-p
            "Returns T if the object is a CASLIST

See CASLIST (type)")
  
  (function to-list
            "Turns the CASLIST in to a standard chain of CONSes.")
  
  (function mapc
            "Calls FUNCTION with each element in the list.

Returns the list.")
  
  (function first
            "Returns the first element in the list, or NIL.")
  
  (function nth
            "Returns the N-th element in the list, or NIL.")
  
  (function length
            "Returns the number of elements in the list.")
  
  (function push
            "Pushes a new element to the front of the list.")
  
  (function delete
            "Removes the first occurrence of VALUE in the list.")
  
  (function member
            "Returns T if the VALUE occurs at least once in the list."))

;;; queue.lisp
(in-package #:org.shirakumo.luckless.queue)
(docs:define-docs
  (type queue
        "A lock-free multiple-writer single-reader queue.

This queue is optimised to allow many writer threads at once and a
single, but efficient-to-iterate reader thread. The queue can resize
to fit more elements, but remains O(1) for PUSH.

See MAKE-QUEUE
See QUEUE-P
See PUSH
See DISCARD
See MAPC
See LENGTH")
  
  (function make-queue
            "Returns a new QUEUE.

The INITIAL-SIZE is a hint as to the capacity of the queue before it
needs to resize.

See QUEUE (type)")
  
  (function queue-p
            "Returns T if the object is a QUEUE")
  
  (function push
            "Pushes ELEMENT onto the queue.")
  
  (function discard
            "Discards all elements currently on the queue.")
  
  (function mapc
            "Maps FUNCTION over all elements in the queue.

Note that this will also consume all elements, leaving the queue empty
after iteration.")
  
  (function length
            "Returns the number of elements currently set in the queue."))
