type t = {
  fn : Object.compiledFunction;
  ip : int;
}

let make fn = { fn; ip = -1 }
let instructions f = f.fn
