type t = {
  fn : Object.compiledFunction;
  ip : int;
  base_pointer : int;
}

let make fn base_pointer = { fn; ip = -1; base_pointer }
let instructions f = f.fn.instructions
