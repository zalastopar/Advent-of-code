let ime_datoteke = "day_9/day_9.in"


let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := int_of_string (input_line chan) :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;
    (*tole je tuja koda iz stackoverflow (dodala sem "int_of_string")- https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)



let rec sestej seznam vsota= match seznam with
  | [] -> []
  | _ :: [] -> []
  | glavna :: x :: rest -> if glavna + x = vsota then (glavna :: x :: []) else sestej (glavna::rest) vsota

let rec preveri_vse seznam vsota = match seznam with
  | [] -> []
  | x :: rest -> if sestej seznam vsota != [] then sestej seznam vsota else preveri_vse rest vsota

let rec index el sez = 
  let rec pomozna st el sez = match sez with
    | [] -> failwith "ni pravega el"
    | a :: rest -> if a = el then st else pomozna (st +1) el rest
  in pomozna 0 el sez

let rec firstk k xs = match xs with
| [] -> failwith "firstk"
| x::xs -> if k=1 then [x] else x::firstk (k-1) xs;;
(*vir: https://stackoverflow.com/questions/26543669/ocaml-return-first-n-elements-of-a-list*)

let rec spusti_prvih k sez cel_sez= match sez with
  | [] -> failwith "napaka"
  | a :: rest ->
  if (index a cel_sez) >= (k-1) then rest else spusti_prvih k rest cel_sez

let rec najdi_napacno seznam = 
  let zacetni = (firstk 25 seznam)
  in
  let ostali = (spusti_prvih 25 seznam seznam)
  in
  let rec pomozna mini sez = match sez with
    | [] -> failwith "cudno"
    | a :: rest -> 
    if (preveri_vse mini a) != [] then pomozna (List.tl (mini @ [a])) rest 
    else a
  in 
  pomozna zacetni ostali

let preveri_ujemanje_od seznam st = 
  let rec pomozna pravi seznam vsota st = match seznam with
    | [] -> failwith "prazen"
    | a :: rest -> if (vsota + a) < st then pomozna (pravi @ [a]) rest (vsota + a) st
    else if (vsota + a) = st then pravi @ [a]
    else []
  in pomozna [] seznam 0 st

let rec poisci_niz seznam stevilka =
  let indeks = index stevilka seznam in
  let manjsi = firstk indeks seznam
  in
  let rec pomozna seznam st = match seznam with
    | [] -> failwith "zanimivo"
    | a :: rest -> 
    if (preveri_ujemanje_od (a::rest) st) != [] then (preveri_ujemanje_od (a::rest) st)
    else pomozna rest st
  in pomozna manjsi stevilka

let poisci_max_in_min sez = 
  let rec pomozna min max sez = match sez with
    | [] -> min + max
    | a :: rest ->
    if a < min then pomozna a max rest
    else if a > max then pomozna min a rest
    else pomozna min max rest
  in pomozna 1000000 0 sez


let naloga1 data = najdi_napacno data
let naloga2 data = 
  let st = najdi_napacno data
  in
  poisci_max_in_min (poisci_niz data st)

let _ =
  let izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = read_file ime_datoteke in
  let odgovor1 = string_of_int (naloga1 vsebina_datoteke)
  and odgovor2 = string_of_int (naloga2 vsebina_datoteke)
  in
  izpisi_datoteko "day_9/day_9_1.out" odgovor1;
  izpisi_datoteko "day_9/day_9_2.out" odgovor2  


