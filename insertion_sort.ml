open Visual_sort

let steps = ref []

let insertion_sort node_list =
  let length = Array.length node_list in
  for i = 0 to length - 1 do
    add_step steps (step_set_sorted node_list i);
    set_sorted node_list.(i);

    add_step steps (step_flag node_list i);
    let new_pos = ref i in
    for j = i downto 1 do
      if (node_list.(j).value < node_list.(j - 1).value) then begin
        add_step steps (step_swap node_list j (j - 1) ~highlight:true);
        add_step steps (step_flag node_list (j - 1));

        swap node_list j (j - 1);
        new_pos := j - 1;
      end;
    done;
   add_step steps (step_restore node_list !new_pos);
  done

let start _ =
  let canvas_elem =
    Js.Opt.get (Html.document##getElementById(Js.string "canvas"))
      (fun () -> assert false) in
  Dom.appendChild canvas_elem canvas;
  Array.iter (fun n -> (draw_node n)) nodes;

  insertion_sort nodes;
  rebuild_nodes nodes;
  
  Html.window##alert (Js.string "Let's start!");
  run_steps !steps;
  Js._false

let _ =
Html.window##onload <- Html.handler start

