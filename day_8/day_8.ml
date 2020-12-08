open Printf

let ime_datoteke = "day_8/day_8.in"



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

let sez_to_tuple sez = match sez with
  | a :: b :: rest ->   (a, int_of_string b)
  | _ -> failwith "napaka"

let rec loci_po_presledku seznam =
  let rec pomozna nov_list = function
  | [] -> nov_list
  | a::rest -> pomozna (nov_list @ [sez_to_tuple (String.split_on_char ' ' a)]) rest
  in
  pomozna [] seznam


let dopisi_st seznam =
  let rec pomozna st sez = match sez with
    | [] -> []
    | ( a, b) :: rest -> (a, b, st) :: pomozna (st + 1) rest
  in
  pomozna 0 seznam

let rec izvedi_operacije seznam =
  let rec pomozna acc preverjene seznam mesto = match (List.nth seznam mesto) with
    | ("acc", st, b) -> 
      if b + 1>= (List.length seznam) then [1; acc + st]
      else if List.mem ("acc", st, b) preverjene then [2; acc]
      else pomozna (acc + st) (preverjene @ [("acc", st, b)]) seznam (b + 1)
    | ("nop", st, b) ->  
      if b + 1>= (List.length seznam) then [1; acc]
      else if List.mem ("nop", st, b) preverjene then [2; acc]
      else pomozna acc (preverjene @ [("nop", st, b)]) seznam (b + 1)
    | ("jmp", st, b) ->  
      if (b + st) >= (List.length seznam) then [1; acc]
      else  
        if List.mem ("jmp", st, b) preverjene then [2; acc]
        else
          pomozna acc (preverjene @ [("jmp", st, b)]) seznam (b + st)
    | _ -> failwith "napaka"
  in pomozna 0 [] seznam 0

let zamenjaj = function
  | ("jmp", st, b) -> ("nop", st, b)
  | ("nop", st, b) -> ("jmp", st, b)
  | _ -> failwith "napaka"

let rec spremeni sez star nov = match sez with
  | [] -> []
  | a :: rest -> if a = star then nov :: (spremeni rest star nov) 
  else a :: (spremeni rest star nov)

let preveri_vse seznam = 
  let rec pomozna mesto seznam = match (List.nth seznam mesto) with
    | ("acc", _, _) -> 
      if (mesto + 1) >= List.length seznam then failwith "cudno"
      else pomozna (mesto + 1) seznam
    | (a, st, b) -> 
      if (List.nth (izvedi_operacije (spremeni seznam (a, st, b) (zamenjaj (a, st, b)))) 0) = 1 then (List.nth (izvedi_operacije (spremeni seznam (a, st, b) (zamenjaj (a, st, b)))) 1)
      else if (mesto + 1) >= List.length seznam then failwith "cudna napaka"
      else pomozna (mesto + 1) seznam
  in
  pomozna 0 seznam



let k = dopisi_st (loci_po_presledku (read_file ime_datoteke))

let naloga1 datoteka =  (List.nth (izvedi_operacije (dopisi_st (loci_po_presledku (datoteka)))) 1)

let naloga2 datoteka = preveri_vse (dopisi_st (loci_po_presledku (datoteka)))


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
  izpisi_datoteko "day_8/day_8_1.out" odgovor1;
  izpisi_datoteko "day_8/day_8_2.out" odgovor2  

