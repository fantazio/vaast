(** Utility functions to manipulate ocaml_XXX values *)

open Core_intf

let until_500 x = Until_500 x
let since_500 x = Since_500 x

let until_510 x = Until_510 x
let since_510 x = Since_510 x

let until_520 x = Until_520 x
let since_520 x = Since_520 x

let until_530 x = Until_530 x
let since_530 x = Since_530 x

let not_available version = version NA

let is_not_available version v =
  assert (v = version NA)

let get_since_500 = function
  | Until_500 NA -> assert false
  | Since_500 x -> x

let get_until_510 = function
  | Until_510 x -> x
  | Since_510 NA -> assert false

let get_since_510 = function
  | Until_510 NA -> assert false
  | Since_510 x -> x

let get_since_520 = function
  | Until_520 NA -> assert false
  | Since_520 x -> x

let get_since_530 = function
  | Until_530 NA -> assert false
  | Since_530 x -> x
