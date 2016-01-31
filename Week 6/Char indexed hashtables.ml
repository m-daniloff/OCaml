(*CHAR INDEXED HASHTABLES  (40 points possible)
Have a look at the documentation of module Hashtbl.

Implement a module CharHashedType, compatible with the HashedType signature, where type t = char.
Use the module defined in the previous question to instantiate the Hashtbl.Make functor as a module CharHashtbl.
Reimplement the data structure of trie from a previous exercise, so that a hash table is used to represent the association between characters and children. To do so, complete the definition of module Trie, so that it is compatible with the given signature GenericTrie, whose 'a table type is instanciated to char indexed hash tables. 
Be careful, a hash table is not a purely functional data structure. Therefore, it must be copied when necessary! 
Note: you must neither change the signature nor the types of module Trie or the tests will fail.
THE GIVEN PRELUDE

*)

module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
end

module CharHashedType =
struct (* replace this structure with your implementation *) end

module CharHashtbl =
struct (* replace this structure with your implementation *) end

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t =
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table

  let empty () =
    "Replace this string with your implementation." ;;

  let lookup trie w =
    "Replace this string with your implementation." ;;

  let insert trie w v =
    "Replace this string with your implementation." ;;

end
