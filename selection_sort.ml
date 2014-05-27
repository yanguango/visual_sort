open Yg_sort

let steps = ref []

let selection_sort node_list =
  let length = Array.length node_list in
  for i = 0 to length - 1 do
    let min = ref i in

    add_step steps (step_flag node_list i); (* flag original min *)

    for j = i + 1 to length - 1 do
      add_step steps (step_compare node_list j);

      if (node_list.(j).value < node_list.(!min).value) then begin 
        if !min <> i then add_step steps (step_restore node_list !min);
        min := j;
        add_step steps (step_flag node_list !min);
      end;
    done;
    if !min <> i then begin
      add_step steps (step_swap node_list i !min);
      swap node_list i !min
    end
    else
      add_step steps (step_restore node_list !min);
    add_step steps (step_flag_sorted node_list i)
  done
   (* Html.window##alert (Js.string (to_string nodes)) *)


let start _ =
  let canvas_elem =
    Js.Opt.get (Html.document##getElementById(Js.string "canvas"))
      (fun () -> assert false) in
  Dom.appendChild canvas_elem canvas;
  Array.iter (fun n -> (draw_node n)) nodes;
  
   (* Html.window##alert (Js.string (to_string nodes)); *)

  selection_sort nodes;
  rebuild_nodes nodes;
  
  (* Html.window##alert (Js.string (to_string nodes)); *)

  Html.window##alert (Js.string (string_of_int (List.length !steps)));
  run_steps !steps;
  Js._false

let _ =
Html.window##onload <- Html.handler start
