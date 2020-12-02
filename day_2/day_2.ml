let ime_datoteke = "day_2/day_2.in"




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





let rec loci_po_presledku seznam =
  let rec pomozna nov_list = function
  | [] -> nov_list
  | a::rest -> pomozna (nov_list @ [String.split_on_char ' ' a]) rest
  in
  pomozna [] seznam

let rec str_to_int seznam = 
  let rec pomozna stevilke = function
    | [] -> stevilke
    | a::rest -> pomozna (stevilke @ [int_of_string a]) rest
  in
  pomozna [] seznam

let prestej el str = match str with
  | [] -> String.length (List.hd str)
  | _ -> List.length (String.split_on_char (String.get el 0) (List.hd str)) -1


let rec dobi_stevilke seznam =
  let rec pomozna nov_list = function
    | [] -> nov_list
    | a::rest -> pomozna (nov_list @ (String.split_on_char '-' a)) rest
  in
  pomozna [] seznam

let uredi_odsek seznam = match seznam with
  | [] -> []
  | st :: crka :: geslo :: rest-> str_to_int (dobi_stevilke [st]) @ [prestej crka [geslo]]
  | _ -> failwith "Napaka1"

let preveri_ujemanje seznam = match seznam with
  | prva :: druga :: presteto :: rest -> if presteto >= prva && presteto <= druga then true else false
  | _ -> failwith "Napaka"

let rec prestej_pravilne seznam = 
  let delno = loci_po_presledku seznam 
  in 
  let rec pomozna stevilo seznam = match seznam with
    | [] -> stevilo
    | a :: rest -> if preveri_ujemanje (uredi_odsek a) then pomozna (stevilo + 1) rest else pomozna stevilo rest
  in 
  pomozna 0 delno

let uredi_odsek_2 seznam = match seznam with
  | [] -> []
  | st :: crka :: geslo :: rest -> 
  ((dobi_stevilke [st]) @ [crka]) @ [geslo]
  | _ -> failwith "Napaka"


let preveri_pozicijo seznam = match seznam with
  | st1 :: st2 :: crka :: geslo :: rest -> 
    if ((String.get crka 0) = String.get geslo ((int_of_string st1) -1) && (String.get crka 0) != String.get geslo ((int_of_string st2) -1)) || ((String.get crka 0) != String.get geslo ((int_of_string st1) -1) && (String.get crka 0) = String.get geslo ((int_of_string st2) -1)) then true else false
  | [] -> false
  | _ -> failwith "Napaka"



let rec prestej_prave_2 seznam = 
  let sez = loci_po_presledku seznam
  in
  let rec pomozna stevilo seznam = match seznam with
    | [] -> stevilo
    | a :: rest -> if preveri_pozicijo (uredi_odsek_2 a) then pomozna (stevilo + 1) rest else pomozna stevilo rest
  in
  pomozna 0 sez


let naloga1 vsebina_datoteke = prestej_pravilne vsebina_datoteke

let naloga2 vsebina_datoteke = prestej_prave_2 vsebina_datoteke


let _ =
  let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan
  in
  let vsebina_datoteke = read_file "day_2/day_2.in" in
  let odgovor1 = string_of_int(naloga1 vsebina_datoteke)
  and odgovor2 = string_of_int(naloga2 vsebina_datoteke)
  in
  izpisi_datoteko "day_2/day_2_1.out" odgovor1;
  izpisi_datoteko "day_2/day_2_2.out" odgovor2  
