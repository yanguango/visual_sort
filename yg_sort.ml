let w = 20.0
let g = 20.0
let p = 3.0
let inteval = 200.0
let size = 20
let canvas_h = 400.0

module Html = Dom_html
let _ = Random.self_init ()

type point = {mutable x: float; mutable y: float}
type rect = {mutable origin: point; mutable width: float; mutable height: float}
type node = {mutable value: int; mutable area: rect; mutable sorted: bool}

let to_string nodes = 
   let result = ref "" in
   Array.iter (fun n -> result := !result ^ (string_of_int n.value) ^ ", ") nodes;
   !result

let less n1 n2 =
  n1.value < n2.value
  
let create_list n =
  Array.init n (fun _ -> (Random.int 99) + 1)

let create_node i v = 
  {value = v; 
   area = {
     origin = 
     {
       x = (float_of_int i) *. (w +. g);
       y = canvas_h -. 10. -. (float_of_int v) *. p
     };
     width = w; 
     height = (float_of_int v) *. p
   };
   sorted = false           
  }

let create_canvas () =
  let d = Html.window##document in
  let c = Html.createCanvas d in
  c##width <- 800;
  c##height <- 400;
  c

let list = create_list size
let nodes = Array.mapi (fun i v -> create_node i v) list
let canvas = create_canvas ()
let ctx = canvas##getContext (Html._2d_)

let update_node node v = 
  node.value <- v;
  node.area.origin.y <- canvas_h -. 10. -. (float_of_int v) *. p;
  node.area.height <- (float_of_int v) *. p

let set_sorted node =
  node.sorted <- true
  
let rebuild_nodes nodes = 
  Array.iteri (fun i n -> (update_node nodes.(i) n)) list


let swap node_list i j = 
  let iv = node_list.(i).value in
  let jv = node_list.(j).value in
  update_node node_list.(i) jv;
  update_node node_list.(j) iv

let add_step steps step =
  steps := !steps @ [step]

let draw_node {value; area} = 
  ctx##fillStyle <- Js.string "gray";
  ctx##fillRect (area.origin.x, area.origin.y, area.width, area.height);
  ctx##fillText (value, area.origin.x +. 3., canvas_h)

let step_restore node_list i = 
  let {value;area;sorted} = node_list.(i) in
  let run _ =
    ctx##clearRect (area.origin.x, area.origin.y, area.width, canvas_h);
    if sorted then ctx##fillStyle <- Js.string "black"
    else  ctx##fillStyle <- Js.string "gray";
    ctx##fillRect (area.origin.x, area.origin.y, area.width, area.height);
    ctx##fillText (value, area.origin.x +. 3., canvas_h);
    () in
  run

let step_flag_sorted node_list i = 
  let {value;area} = node_list.(i) in
  let run _ =
    set_sorted node_list.(i);
    ctx##fillStyle <- Js.string "black";
    ctx##fillRect (area.origin.x, area.origin.y, area.width, area.height);
    () in
  run

let step_set_sorted node_list i =
  let run _ =
   set_sorted node_list.(i);
   () in
  run

let step_compare node_list i =
  let {value;area} = node_list.(i) in
  let run _ = 
    ctx##fillStyle <- Js.string "green";
    ctx##fillRect (area.origin.x, area.origin.y, area.width, area.height);
    Html.window##setTimeout ((step_restore node_list i),  inteval);
    () in
  run

let step_compare_two node_list i j = 
  let run _ = 
    ctx##fillStyle <- Js.string "green";
    let {value;area} = node_list.(i) in
    ctx##fillRect (area.origin.x, area.origin.y, area.width, area.height);
    Html.window##setTimeout ((step_restore node_list i),  inteval);
    let {value;area} = node_list.(j) in
    ctx##fillRect (area.origin.x, area.origin.y, area.width, area.height);
    Html.window##setTimeout ((step_restore node_list i),  inteval);
    () in
  run


let step_flag node_list i =
  let {value;area} = node_list.(i) in
  let run _ = 
    ctx##fillStyle <- Js.string "red";
    ctx##fillRect (area.origin.x, area.origin.y, area.width, area.height);
    () in
  run

let redraw_node node_list i ?(highlight=true) = 
  let node = node_list.(i) in
  let {value;area} = node in
  ctx##clearRect (area.origin.x, 0., area.width, canvas_h);

  if highlight then ctx##fillStyle <- Js.string "yellow";
  ctx##fillRect (area.origin.x, area.origin.y, area.width, area.height);
  ctx##fillText (value, area.origin.x +. 3., canvas_h)

let step_swap node_list i j ?(highlight=true) =
  let run _ =
    swap node_list i j;
    redraw_node node_list i ~highlight:highlight;
    redraw_node node_list j ~highlight:highlight;
    if highlight then begin
      Html.window##setTimeout ((step_restore node_list i), inteval);
      Html.window##setTimeout ((step_restore node_list j), inteval);
      ()
    end;
    () in
  run

let rec run_steps = function
  | [] -> ()
  | h::tl -> 
    h ();
    Html.window##setTimeout ((fun _ -> (run_steps tl)), inteval);
  ()
