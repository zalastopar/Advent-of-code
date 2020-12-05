let ime_datoteke = "day_5/day_5.in"

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

let sedezi1 = read_file ime_datoteke

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
(*vir: http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings*)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)
(*vir: https://stackoverflow.com/questions/16950687/integer-exponentiation-in-ocaml *)

let rec dobi_row sedez = 
  let rec pomozna sedez nov = match sedez with
    | [] -> nov
    | a :: rest -> if a = 'R' || a = 'L' then pomozna rest (nov @ [a]) else pomozna rest nov
  in
  pomozna sedez []



let rec ugotovi_vrsto sedez = 
  let sez = explode sedez
  in
  let rec pomozna vsota sedez = match sedez with
    | a :: b :: c :: [] -> vsota
    | a :: rest -> if a = 'F' then pomozna vsota rest else pomozna (vsota + (pow 2 (List.length rest - 3))) rest
    | _ -> failwith "Napaka"
  in
  pomozna 0 sez


let rec ugotovi_sedez sedez = 
  let sez = dobi_row (explode sedez) 
  in
  let rec pomozna vsota sedez = match sedez with
    | [] -> vsota
    | a :: rest -> if a = 'L' then pomozna vsota rest else pomozna (vsota + (pow 2  (List.length rest))) rest
  in
  pomozna 0 sez

let izracunaj_id sedez = (ugotovi_vrsto sedez) * 8 + (ugotovi_sedez sedez)

let rec poisci_najvecjega seznam = 
  let rec pomozna seznam st = match seznam with 
    | [] -> st
    | a :: rest -> if (izracunaj_id a) > st then pomozna rest (izracunaj_id a) else pomozna rest st
  in
  pomozna seznam 0

let rec vsi_id seznam = 
  let rec pomozna seznam id = match seznam with
    | [] -> id
    | a :: rest -> pomozna rest (id @ [izracunaj_id a])
  in
  pomozna seznam []




let sedezi = List.sort compare (vsi_id (read_file ime_datoteke))


let rec odstej seznam = 
  let rec pomozna seznam dobri = match seznam with 
    | [] -> dobri
    | _ :: [] -> dobri
    | glavna :: x :: rest -> if x - glavna = 2  then pomozna (glavna :: rest) (dobri @ [(glavna, x)])  else pomozna (glavna::rest) dobri
  in 
  pomozna seznam []

let rec preveri_vse pravi seznam = match seznam with
  | [] -> pravi
  | x :: rest -> preveri_vse (pravi @ (odstej seznam)) rest  

let dobi_tvoj_indeks sez vsi =
  let rec pomozna sez vsi pravi = match sez with
    | [] -> pravi
    | (prvi, drugi) :: rest ->  if (List.mem (prvi + 1) vsi) then pomozna rest vsi pravi
    else pomozna rest vsi (pravi + prvi + 1)
  in 
  pomozna sez vsi 0


let naloga1 seznam = poisci_najvecjega seznam

let naloga2 seznam = dobi_tvoj_indeks (preveri_vse [] (List.sort compare (vsi_id (read_file ime_datoteke)))) (List.sort compare (vsi_id (read_file ime_datoteke)))


 
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
  izpisi_datoteko "day_5/day_5_1.out" odgovor1;
  izpisi_datoteko "day_5/day_5_2.out" odgovor2  





