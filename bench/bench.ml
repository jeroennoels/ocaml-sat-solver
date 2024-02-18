open Core_bench

let line = "Lorem ipsum dolor sit amet"

let () =
  [ Bench.Test.create ~name:"words" (fun () -> ignore (Sat.Util.words line)) ]
  |> Bench.bench
;;
