module Result = struct
  let ( let+ ) r f = Result.map f r
  let ( let* ) = Result.bind
end
