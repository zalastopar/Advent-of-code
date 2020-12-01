open Printf

let ime_datoteke = "day_1/day_1.in"



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

let rec preveri_vse seznam = match seznam with
  | [] -> []
  | x :: rest -> if sestej seznam 2020 != [] then sestej seznam 2020 else preveri_vse rest

let rec prod seznam = match seznam with
  | [] -> 1
  | a :: [] -> a
  | a :: rest -> a * (prod rest)


let rec prvi_z_vsakim seznam = 
  let rec pomozna sez vsote = match sez with
    | [] -> vsote
    | a :: b :: rest -> pomozna (a::rest) (vsote @ [(a + b, a * b)])
    | a :: [] -> vsote
  in
  pomozna seznam []


let rec preveri_ujemanje a vsote = match vsote with
  | [] -> []
  | (x, y) :: rest -> if a + x = 2020 then [a * y] else preveri_ujemanje a rest


let rec preveri_3 seznam = match seznam with
  | [] -> []
  | a :: b :: rest -> if preveri_ujemanje b (prvi_z_vsakim seznam) != [] then preveri_ujemanje b (prvi_z_vsakim seznam) else preveri_3 (a :: rest)
  | a :: [] -> []  

let rec preveri_vse_2 seznam = match seznam with
  | [] -> []
  | a :: rest -> if preveri_3 seznam != [] then preveri_3 seznam else preveri_vse_2 rest

let vrni_string sez = match sez with
  | [] -> "0"
  | a::rest -> string_of_int(a)


let naloga1 vsebina_datoteke = prod (preveri_vse vsebina_datoteke)

let naloga2 vsebina_datoteke = preveri_vse_2 vsebina_datoteke


  
let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan
  in
  let vsebina_datoteke = read_file "day_1/day_1.in" in
  let odgovor1 = string_of_int(naloga1 vsebina_datoteke)
  and odgovor2 = vrni_string(naloga2 vsebina_datoteke)
  in
  izpisi_datoteko "day_1/day_1_1.out" odgovor1;
  izpisi_datoteko "day_1/day_1_2.out" odgovor2

