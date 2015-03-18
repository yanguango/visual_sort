open Visual_sort

let steps = ref []

let rec quick_sort_rec node_list head tail =
  if (tail - head <= 0) then ()
  else begin
    let pivot = node_list.(head).value in
    
    let i = ref head in
    let j = ref tail in
    while (!i <= !j) do
      while (node_list.(!i).value < pivot) do
        add_step steps (step_compare node_list !i);
        i := !i + 1;
      done;
      while (node_list.(!j).value > pivot) do
        add_step steps (step_compare node_list !j);
        j := !j - 1 
      done;
      if (!i <= !j) then begin
        add_step steps (step_swap node_list !i !j ~highlight:true);
        swap node_list !i !j;
        i := !i + 1;
        j := !j - 1;
      end;

    done;
    quick_sort_rec node_list head !j;
    quick_sort_rec node_list !i tail;
    ()
  end


let quick_sort node_list = 
  let length = Array.length node_list in
  quick_sort_rec node_list 0 (length - 1)

let start _ =
  let canvas_elem =
    Js.Opt.get (Html.document##getElementById(Js.string "canvas"))
      (fun () -> assert false) in
  Dom.appendChild canvas_elem canvas;
  Array.iter (fun n -> (draw_node n)) nodes;

  quick_sort nodes;
  rebuild_nodes nodes;
  
  Html.window##alert (Js.string "Let's start!");
  run_steps !steps;
  Js._false

let _ =
Html.window##onload <- Html.handler start

