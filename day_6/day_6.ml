let ime_datoteke = "day_6/day_6.in"


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


let sez = read_file ime_datoteke

let rec do_praznega seznam =
  let rec pomozna popravljen nov sez = match sez with
    | a :: rest -> if String.length a = 0 then pomozna (popravljen @ [nov]) [] rest else pomozna popravljen (nov @ [a]) rest
    | [] -> popravljen @ [nov]
  in
  pomozna [] [] seznam

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) ((Char.escaped s.[i]) :: l) in
  exp (String.length s - 1) []
(*vir: http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings , malo popravljena*)

let explode1 s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
(*vir: http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings , malo popravljena*)

let rec uredi_skupino seznam =
  let rec pomozna sez urejeni = match sez with
    | [] -> urejeni
    | a :: rest -> pomozna rest (urejeni @ explode a)
  in
  pomozna seznam []

let count_unique_elements_naive list = 
  let count_element e list = List.filter (fun x -> x = e) list |> List.length in
  List.sort_uniq String.compare list 
  |> List.map (fun e -> (e, count_element e list))
(*vir: https://www.thekerneltrip.com/ocamli-snippets/count-unique-elements-in-list-ocaml/ , malo popravljena*)

let odg = do_praznega (read_file ime_datoteke)

let st_skupine seznam = List.length (count_unique_elements_naive (uredi_skupino seznam))

let prestej_skupine seznam = 
  let rec pomozna vsota seznam = match seznam with
    | [] -> vsota
    | a :: rest -> pomozna (vsota + st_skupine a) rest
  in
  pomozna 0 seznam

let rec prvi_v_vseh el seznam = match seznam with
    | [] -> 1
    | a :: rest -> if String.contains a el then prvi_v_vseh el rest else 0


let rec se_prestej seznam = 
  let prvi = explode1 (List.hd seznam)
  in
  let rec pomozna prvi vsota seznam = match prvi with
    | [] -> vsota
    | a :: rest -> pomozna rest (vsota + (prvi_v_vseh a seznam)) seznam
  in
  pomozna prvi 0 seznam

let prestej_vse seznam = 
  let rec pomozna vsota seznam = match seznam with
    | [] -> vsota
    | a :: rest -> pomozna (vsota + (se_prestej a)) rest
  in
  pomozna 0 seznam

let naloga1 datoteka = prestej_skupine (do_praznega datoteka)

let naloga2 datoteka = prestej_vse (do_praznega datoteka)

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
  izpisi_datoteko "day_6/day_6_1.out" odgovor1;
  izpisi_datoteko "day_6/day_6_2.out" odgovor2  