let array = [|2;4;1;3;5|];;

let swap array i j = 
  let temp = array.(i) in
  array.(i) <- array.(j);
  array.(j) <- temp;;

let selection_sort array =
  let length = Array.length array in
  for i = 0 to length - 1 do
    let min_index = i in
    for j = i + 1 to length - 1 do
      if (array.(j) < array.(min_index)) then let min_index = j in
      swap array i min_index
    done
  done;;

Array.iter (fun n -> print_int n) array;;
selection_sort array;;
print_string "\n";;
Array.iter (fun n -> print_int n) array;;