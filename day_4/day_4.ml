let ime_datoteke = "day_4/day_4.in"


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



let rec do_praznega seznam =
  let rec pomozna popravljen nov sez = match sez with
    | a :: rest -> if String.length a = 0 then pomozna (popravljen @ [nov]) [] rest else pomozna popravljen (nov @ [a]) rest
    | [] -> popravljen @ [nov]
  in
  pomozna [] [] seznam

  
let rec loci_po_presledku seznam =
  let rec pomozna nov_list = function
  | [] -> nov_list
  | a::rest -> pomozna (nov_list @ String.split_on_char ' ' a) rest
  in
  pomozna [] seznam

let rec loci_po_dvopicju seznam =
  let rec pomozna nov_list = function
  | [] -> nov_list
  | a::rest -> pomozna (nov_list @ String.split_on_char ':' a) rest
  in
  pomozna [] seznam

let if_cm str = 
  if (String.concat "" [Char.escaped (String.get str ((String.length str) -2)); Char.escaped (String.get str ((String.length str)-1))]) = "cm" then "cm"
  else if (String.concat "" [Char.escaped (String.get str ((String.length str) -2)); Char.escaped (String.get str ((String.length str)-1))]) = "in" then "in"
  else ""

let rec uredi seznam = match seznam with
  | [] -> []
  | a :: rest -> [loci_po_dvopicju (loci_po_presledku a)] @ uredi rest

let cid seznam = List.mem "cid" seznam
let pid seznam = List.mem "pid" seznam
let ecl seznam = List.mem "ecl" seznam
let hcl seznam = List.mem "hcl" seznam
let hgt seznam = List.mem "hgt" seznam
let eyr seznam = List.mem "eyr" seznam
let iyr seznam = List.mem "iyr" seznam
let byr seznam = List.mem "byr" seznam

let ujemanje seznam = byr seznam && pid seznam && ecl seznam && hcl seznam && hgt seznam && eyr seznam && iyr seznam

let rec dobi_prave seznam = 
  let rec pomozna pr seznam = match seznam with
    | [] -> pr
    | a :: rest -> if ujemanje a then pomozna (pr @ [a]) rest else pomozna pr rest
  in
  pomozna [] seznam

let rec index seznam el = 
  let rec pomozna el seznam = match seznam with
  | [] -> ""
  | a :: b :: rest -> if a = el then b else pomozna el (b :: rest)
  | _ :: [] -> ""
  in
  pomozna el seznam

let check_int s = 
  try int_of_string s |> ignore; true
  with Failure _ -> false 
(*https://stackoverflow.com/questions/43554262/how-to-validate-if-a-string-only-contains-number-chars-in-ocaml*)

let rec uredi2 seznam = match seznam with
  | [] -> []
  | a :: rest -> [[(loci_po_presledku a)]] @ [uredi rest]

let get_check_num str = match str with
  | _ -> if (if_cm str) = "cm" then check_int(List.hd (String.split_on_char 'c' str))
    else if (if_cm str) = "in" then check_int(List.hd (String.split_on_char 'i' str))
    else false

let is_alpha = function 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
(*obe iz : https://stackoverflow.com/questions/49184057/does-ocaml-have-a-module-that-is-like-isdigit-and-isalpha-in-c-c*)

let rec je_barva str =
  let dolzina = String.length str
  in
  let rec pomozna i str = match i with
    | _ -> 
    if i > (dolzina - 1 )then true
    else if (is_alpha (String.get str (i)) || is_digit (String.get str (i))) && (String.length str) = 7 then pomozna (i+1) str
    else false
  in
  pomozna 1 str




let preveri_byr seznam = if check_int (index seznam "byr") then int_of_string ((index seznam "byr")) >= 1920 && int_of_string ((index seznam "byr")) <= 2002 else false

let preveri_iyr seznam = if check_int (index seznam "iyr") then int_of_string ((index seznam "iyr")) >= 2010 && int_of_string ((index seznam "iyr")) <= 2020 else false

let preveri_eyr seznam = if check_int (index seznam "eyr") then int_of_string ((index seznam "eyr")) >= 2020 && int_of_string ((index seznam "eyr")) <= 2030 else false

let preveri_hgt seznam = match if_cm (index seznam "hgt") with
  | "cm" -> if get_check_num (index seznam "hgt") then int_of_string (List.hd (String.split_on_char 'c' (index seznam "hgt"))) >= 150 && int_of_string (List.hd (String.split_on_char 'c' (index seznam "hgt"))) <= 193 else false
  | "in" -> if get_check_num (index seznam "hgt") then int_of_string (List.hd (String.split_on_char 'i' (index seznam "hgt"))) >= 59 && int_of_string (List.hd (String.split_on_char 'i' (index seznam "hgt"))) <= 76 else false
  | _ -> false

let preveri_ecl seznam = List.mem (index seznam "ecl") ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]

let preveri_pid seznam = String.length (index seznam "pid") = 9 && check_int (index seznam "pid")

let preveri_hcl seznam = (String.get (index seznam "hcl") 0) = '#' && je_barva (index seznam "hcl")

let rec prestej_prave i seznam = match seznam with 
  | [] -> i
  | a :: rest -> 
  if (preveri_byr a) && (preveri_eyr a) && (preveri_iyr a) && (preveri_hgt a) && (preveri_ecl a) && (preveri_pid a) && (preveri_hcl a) then prestej_prave (i+1) rest 
  else prestej_prave i rest

let naloga1 datoteka = List.length (dobi_prave (uredi (do_praznega datoteka)))

let naloga2 datoteka = prestej_prave 0 (dobi_prave (uredi (do_praznega datoteka))) 


let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan
  in
  let vsebina_datoteke = read_file "day_4/day_4.in" in
  let odgovor1 = string_of_int(naloga1 vsebina_datoteke)
  and odgovor2 = string_of_int(naloga2 vsebina_datoteke)
  in
  izpisi_datoteko "day_4/day_4_1.out" odgovor1;
  izpisi_datoteko "day_4/day_4_2.out" odgovor2  


