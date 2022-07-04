type t =
  | Integer of int
  | Boolean of bool
  | Null
  | Return  of t
[@@deriving show, eq]
