type 'a t = (Syntax.id * 'a) list

exception Not_bound

let empty = []

let extend x v env = (x,v)::env

let extendback x v env = env @ [(x, v)]

let rec lookup x env =
  try List.assoc x env with Not_found -> raise Not_bound

let rec map f = function
    [] -> []
  | (id, v)::rest -> (id, f v) :: map f rest

let rec fold_right f env a =
  match env with
    [] -> a
  | (_, v)::rest -> f v (fold_right f rest a)

let rec print_env f env ff = 
  match env with
      [] -> ()
    | (id, v)::rest -> 
        if ff v then
          begin
            Printf.printf "val %s = " id;
            f v;
            print_newline();
            print_env f rest ff
          end
        else
          begin
            print_env f rest ff
          end
(* let rec length = function
    [] -> 0
  | _ :: rest -> 1 + length rest *)