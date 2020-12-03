let ime_datoteke = "day_3/day_3.in"



let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;
    (*tole je tuja koda iz stackoverflow (dodala sem "int_of_string")- https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)

let pot = read_file ime_datoteke

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
(*vir: http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings*)

let rec razbij seznam =
  let rec pomozna novo sez = match sez with
    | [] -> novo
    | a :: rest -> pomozna (novo @ [explode a]) rest
  in
  pomozna [] seznam


let rec premik_3_1 pozicija seznam = 
  let rec pomozna drevesa seznam pozicija = match pozicija with
    | (i, j) -> 
    if i >= (List.length seznam) - 1 then drevesa 
    else if j <= 27 then if (List.nth (List.nth seznam (i +1)) (j + 3)) = '#' then pomozna (drevesa + 1) seznam (i + 1, j + 3) else pomozna drevesa seznam (i + 1, j + 3)
    else if (List.nth (List.nth seznam (i + 1)) (j + 3 - 31)) = '#' then pomozna (drevesa + 1) seznam (i + 1, j - 31 + 3 ) else pomozna drevesa seznam (i + 1, j - 31+ 3)
  in
  pomozna 0 seznam pozicija
(*220*)

let rec premik pozicija seznam dol desna =
  let rec pomozna drevesa seznam pozicija dol desna = match pozicija with
  | (i, j) -> 
  if i >= ((List.length seznam) - dol) then drevesa 
  else if j <= (30 - desna) then if (List.nth (List.nth seznam (i + dol)) (j + desna)) = '#' then pomozna (drevesa + 1) seznam (i + dol, j + desna) dol desna else pomozna drevesa seznam (i + dol, j + desna) dol desna
  else if (List.nth (List.nth seznam (i + dol)) (j + desna - 31)) = '#' then pomozna (drevesa + 1) seznam (i + dol, j + desna - 31) dol desna else pomozna drevesa seznam (i + dol, j + desna - 31) dol desna
  in
  pomozna 0 seznam pozicija dol desna

let a = premik (0,0) (razbij pot) 1 1
let b = premik (0,0) (razbij pot) 1 3
let c = premik (0,0) (razbij pot) 1 5
let d = premik (0,0) (razbij pot) 1 7
let e = premik (0,0) (razbij pot) 2 1

let naloga1 vsebina = premik (0,0) (razbij vsebina) 1 3

let naloga2 vsebina = (premik (0,0) (razbij vsebina) 1 1)*(premik (0,0) (razbij vsebina) 1 3) * (premik (0,0) (razbij vsebina) 1 5) * (premik (0,0) (razbij vsebina) 1 7) * (premik (0,0) (razbij vsebina) 2 1)

let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan
  in
  let vsebina_datoteke = read_file "day_3/day_3.in" in
  let odgovor1 = string_of_int(naloga1 vsebina_datoteke)
  and odgovor2 = string_of_int(naloga2 vsebina_datoteke)
  in
  izpisi_datoteko "day_3/day_3_1.out" odgovor1;
  izpisi_datoteko "day_3/day_3_2.out" odgovor2  