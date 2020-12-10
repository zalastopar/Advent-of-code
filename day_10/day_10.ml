let ime_datoteke = "day_10/day_10.in"


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



let prestej_razlike sez =
  let s = List.sort compare sez in
  let rec pomozna sez ena tri = match sez with
    | [] -> failwith "napaka"
    | a :: b :: rest ->
    if (b - a) = 1 then pomozna (b ::rest) (ena + 1) tri
    else if (b -a) = 3 then pomozna (b :: rest) ena (tri+1)
    else pomozna (b :: rest) ena tri
    | a :: [] -> (ena + 1) * (tri + 1)
  in pomozna s 0 0

 (*dobi index el*) 
let rec index el sez = 
  let rec pomozna st el sez = match sez with
    | [] -> failwith "ni pravega el"
    | a :: rest -> if a = el then st else pomozna (st +1) el rest
  in pomozna 0 el sez


(*cleni, ki jih lahko izbrisemo*)
let izloci_enega seznam =
  let rec pomozna sez lahko sez1  = match sez with
    | [] -> lahko
    | a :: b :: c :: rest -> if (index a sez1) = 0 then (if b <= 3 then pomozna (b::c::rest) (lahko @ [a]) sez1 else pomozna (b::c::rest) lahko sez1)
    else if (c - a) <= 3 then pomozna (b :: c :: rest) (lahko @ [b]) sez1
    else pomozna (b::c::rest) lahko sez1
    | a :: rest -> pomozna rest lahko sez1
  in
  pomozna seznam [] seznam

(*generira vse kombinacije, če iz lista iz k clenov*)
(*vir: https://ocaml.org/learn/tutorials/99problems.html*)
let rec extract k list =
  if k <= 0 then [ [] ]
  else match list with
        | [] -> []
        | h :: tl ->
          let with_h = List.map (fun l -> h :: l) (extract (k-1) tl) in
          let without_h = extract k tl in
          with_h @ without_h;;

let moc_mnozice seznam = 
  let rec pomozna moc k seznam = match k with
    | -1 -> moc
    | a -> pomozna (moc @ (extract k seznam)) (k - 1) seznam
  in pomozna [] (List.length seznam) seznam
    
 (*izloci podmnozice, ki imajo 3 zaporedne stevilke*) 
let izloci_napacne sez = 
  let rec pomozna prejsna st sez = match sez with 
    | [] -> st < 2
    | a :: rest -> if a = prejsna + 1 then (if st + 1 = 2 then false else pomozna a (st + 1) rest) else pomozna a 0 rest
  in 
  let rec prestej sez st = match sez with
    | [] -> st
    | a :: rest -> if a = [] then prestej rest (st +1)
    else if pomozna (List.hd a) 0 a then prestej rest (st + 1) 
    else prestej rest st
  in
  prestej sez 0
  

(*loci seznam ker se stevilki razlikujeta za vsaj 3*)
let skoki seznam =
  let rec pomozna novi sez trenutni = match sez with
    | [] -> List.tl (novi @ [List.sort compare trenutni])
    | a :: rest -> if a = (List.hd trenutni) + 1 then pomozna novi rest (a :: trenutni) else pomozna (novi @ [List.sort compare trenutni]) rest [a]
  in 
  pomozna [] seznam [List.hd seznam]

(*preveri vse nacine, kako lahko odstranimo elemente*)
let zmnozi seznam = 
  let skok = skoki seznam in
  let rec vse_kombinacije st sez = match sez with
    | [] -> st
    | a :: rest -> vse_kombinacije (st * (izloci_napacne (moc_mnozice a))) rest
  in vse_kombinacije 1 skok



let naloga1 data = prestej_razlike data


let naloga2 data = zmnozi (izloci_enega (List.sort compare data))

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
  izpisi_datoteko "day_10/day_10_1.out" odgovor1;
  izpisi_datoteko "day_10/day_10_2.out" odgovor2 