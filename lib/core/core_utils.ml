(** Utility functions to manipulate ocaml_XXX values *)

open Core_intf

let until_500 x = Until_500 x
let since_500 x = Since_500 x

let until_501 x = Until_501 x
let since_501 x = Since_501 x

let until_502 x = Until_502 x
let since_502 x = Since_502 x

let until_503 x = Until_503 x
let since_503 x = Since_503 x

let until_504 x = Until_504 x
let since_504 x = Since_504 x

let until_505 x = Until_505 x
let since_505 x = Since_505 x

let not_available version = version NA

let is_not_available version v =
  assert (v = version NA)

let get_since_500 = function
  | Until_500 NA -> assert false
  | Since_500 x -> x

let get_until_501 = function
  | Until_501 x -> x
  | Since_501 NA -> assert false

let get_since_501 = function
  | Until_501 NA -> assert false
  | Since_501 x -> x

let get_since_502 = function
  | Until_502 NA -> assert false
  | Since_502 x -> x

let get_since_503 = function
  | Until_503 NA -> assert false
  | Since_503 x -> x

let get_since_504 = function
  | Until_504 NA -> assert false
  | Since_504 x -> x

let get_since_505 = function
  | Until_505 NA -> assert false
  | Since_505 x -> x
