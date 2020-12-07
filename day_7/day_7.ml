#load "str.cma"
open Str

let ime_datoteke = "day_7/day_7.in"

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := (input_line chan) :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;
    (*tole je tuja koda iz stackoverflow - https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml *)


let rec loci_po_presledku seznam =
  let rec pomozna nov_list = function
  | [] -> nov_list
  | a::rest -> pomozna (nov_list @ [String.split_on_char ' ' a]) rest
  in
  pomozna [] seznam

let is_digit = function '0' .. '9' -> true | _ -> false
(*iz : https://stackoverflow.com/questions/49184057/does-ocaml-have-a-module-that-is-like-isdigit-and-isalpha-in-c-c*)

let int_of_char ch = int_of_string (Char.escaped ch)



let dobi_barvo str = 
  let s = String.split_on_char ' ' str
  in
  let rec pomozna sez nov = match sez with
    | [] -> String.concat " " nov
    | a :: rest -> if is_digit (String.get a 0) then pomozna rest nov else pomozna rest (nov @ [a])
  in
  pomozna s []

let rec delete el sez = 
  let rec pomozna dobri el sez = match sez with 
    | [] -> dobri
    | a :: rest -> if a = el then pomozna dobri el rest else pomozna (dobri @ [a]) el rest
  in
  pomozna [] el sez

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try 
     ignore (Str.search_forward re s1 0); 
     true
  with Not_found -> false
(*vir: https://stackoverflow.com/questions/11193783/ocaml-strings-and-substrings*)

let rec uredi seznam = 
  let rec pomozna seznam nov barva = match seznam with
    | [] -> nov
    | a :: rest -> 
    if a = "bags" then pomozna rest (nov @ [String.concat " " barva]) [] 
    else if a = "contain" then pomozna rest nov barva
    else if (a = "bags," || a = "bag," || a = "bag." ||a = "bags." ) then pomozna rest (nov @ [String.concat " " barva]) []
    else pomozna rest nov (barva @ [a])
  in
  pomozna seznam [] []

let preveri_torbo barva seznam = 
  let notranje = List.tl seznam in 
  let glavna = List.hd seznam in
  let rec preveri barva sez = match sez with
    | [] -> ""
    | a :: rest -> if contains a barva then glavna else preveri barva rest
  in 
  preveri barva notranje

let rec zdruzi_sez s1 s2 s3 = match s3 with
  | [] -> s1
  | a :: rest -> 
  if List.mem a s1 || List.mem a s2 then zdruzi_sez s1 s2 rest 
  else zdruzi_sez (s1 @ [a]) s2 rest
 

let rec preveri_cel_sez sez torba = match sez with
  | [] -> []
  | a :: rest -> if preveri_torbo torba a = "" then preveri_cel_sez rest torba else (preveri_torbo torba a) ::  (preveri_cel_sez rest torba)
  

let prestej seznam moja = 
  let rec pomozna potrebne seznam preverjene = match potrebne with
    | [] -> List.length preverjene - 1
    | a :: rest -> pomozna (delete a (zdruzi_sez potrebne preverjene (preveri_cel_sez seznam a))) seznam (preverjene @ [a])
  in 
  pomozna [moja] seznam []
  

let torbe file = 
  let sez = loci_po_presledku (read_file file) in
  let rec pomozna sez = match sez with
    | [] -> []
    | a :: rest -> (uredi a) :: pomozna rest
  in 
  pomozna sez


let rec preuredi del = 
  let rec pomozna del novo = match del with
    | [] -> novo
    | a :: rest -> 
      if is_digit (String.get a 0) then pomozna rest (novo @ [(int_of_char (String.get a 0), dobi_barvo a)])
      else pomozna rest (novo @ [(1, a)])
  in
  pomozna del []

let rec preuredi_vse seznam = match seznam with
  | [] -> []
  | a :: rest -> preuredi a :: preuredi_vse rest

let dobi_drugi_el sez = match sez with
  | (st, b) :: rest -> b
  | _ -> failwith "napaka"

let rec dobi_vsebino bag seznam = match seznam with
  | [] -> []
  | a :: rest -> if (dobi_drugi_el a) = bag then List.tl a else dobi_vsebino bag rest

let k = preuredi_vse (torbe ime_datoteke)




let k = preuredi_vse (torbe ime_datoteke)




let naloga1 datoteka = prestej (torbe datoteka) "shiny gold"