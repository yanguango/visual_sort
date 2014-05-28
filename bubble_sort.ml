open Yg_sort

let steps = ref []

let bubble_sort node_list =
  let length = Array.length node_list in
  for i = 0 to length - 1 do
    for j = length - 1 downto (i + 1) do
      if (node_list.(j).value < node_list.(j - 1).value) then begin 
        add_step steps (step_swap node_list j (j - 1) ~highlight:true);
        swap node_list j (j - 1)
      end else add_step steps (step_compare_two node_list j (j - 1));
    done;
    add_step steps (step_flag_sorted node_list i)
  done

let start _ =
  let canvas_elem =
    Js.Opt.get (Html.document##getElementById(Js.string "canvas"))
      (fun () -> assert false) in
  Dom.appendChild canvas_elem canvas;
  Array.iter (fun n -> (draw_node n)) nodes;
  
  bubble_sort nodes;
  rebuild_nodes nodes;
  
  Html.window##alert (Js.string (string_of_int (List.length !steps)));
  run_steps !steps;
  Js._false

let _ =
Html.window##onload <- Html.handler start


