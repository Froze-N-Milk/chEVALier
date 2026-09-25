open String

let () = parse "(+ 1 2)"
let () = parse "[+ 1 2]"
let () = parse "[+ 1 2)"
let () = parse "'#hello"
let () = parse "[ '() ]"
let () = parse "'()"
let () = parse "'(+ 1 2)"
let () = parse "prefix(+ 1 2)"
let () = parse "'#prefix(+ 1 2)"
let () = parse "( '#prefix(+ 1 2) '#prefix(+ 1 2) )"
