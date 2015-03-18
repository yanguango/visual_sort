open Visual_sort

let steps = ref []

let shell_sort node_list =
  let length = Array.length node_list in
  let h = ref 1 in
  while (!h < length / 3) do
    h := 3 * !h + 1
  done;
  while (!h >= 1) do
    for i = 0 to (!h - 1) do
      let j = ref i in
      
      while (!j) < length do
        add_step steps (step_flag node_list !j);
        let pos = ref !j in
        while (!pos - !h) >= 0 do
          if (less node_list.(!pos) node_list.(!pos - !h)) then begin
            add_step steps (step_compare node_list (!pos - !h));
            add_step steps (step_swap node_list !pos (!pos - !h) ~highlight:true);
            swap node_list !pos (!pos - !h);
          end;
          pos := !pos - !h
        done;
        add_step steps (step_restore node_list !j);
        j := !j + !h
      done;
    done;
    h := !h / 3
  done
  
let start _ =
  let canvas_elem =
    Js.Opt.get (Html.document##getElementById(Js.string "canvas"))
      (fun () -> assert false) in
  Dom.appendChild canvas_elem canvas;
  Array.iter (fun n -> (draw_node n)) nodes;

  shell_sort nodes;
  rebuild_nodes nodes;
  
  Html.window##alert (Js.string "Let's start!");
  run_steps !steps;
  Js._false

let _ =
Html.window##onload <- Html.handler start


