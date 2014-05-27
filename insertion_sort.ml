open Yg_sort

let steps = ref []

let insertion_sort node_list =
  let length = Array.length node_list in
  for i = 0 to length - 1 do
    add_step steps (step_set_sorted node_list i);
    set_sorted node_list.(i);

    add_step steps (step_flag node_list i);
    let new_pos = ref i in
    for j = i downto 1 do
      (* add_step steps (step_compare_two node_list j (j - 1)); *)

      if (node_list.(j).value < node_list.(j - 1).value) then begin
        (* add_step steps (step_restore node_list j);  *)
        add_step steps (step_swap node_list j (j - 1) ~highlight:true);
        add_step steps (step_flag node_list (j - 1));

        swap node_list j (j - 1);
        new_pos := j - 1;
        
      end;
    done;
   add_step steps (step_restore node_list !new_pos);
    
  done

let start _ =
  Dom.appendChild Html.window##document##body canvas;
  Array.iter (fun n -> (draw_node n)) nodes;
  
   (* Html.window##alert (Js.string (to_string nodes)); *)

  insertion_sort nodes;
  rebuild_nodes nodes;
  
  (* Html.window##alert (Js.string (to_string nodes)); *)

  Html.window##alert (Js.string (string_of_int (List.length !steps)));
  run_steps !steps;
  Js._false

let _ =
Html.window##onload <- Html.handler start

