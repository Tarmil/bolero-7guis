module Sevenguis.List

let replaceAt i repl l =
    let rec loop acc i = function
        | [] -> l
        | x :: xs ->
            if i = 0 then
                List.rev acc @ repl :: xs
            else
                loop (x :: acc) (i - 1) xs
    loop [] i l
