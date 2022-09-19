type t = {
  cl : Object.closure;
  ip : int;
  base_pointer : int;
}

let make cl base_pointer = { cl; ip = -1; base_pointer }
let instructions f = f.cl.Object.compiled_fn.instructions
