type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;


let breadthBT rootNode =
  let rec search visited queue =
    match queue with
      [] -> (List.rev visited)
    | [Empty] -> []
    | h::t -> let v = match h with Node (v, _, _) -> v 
              in
              if (List.mem v visited) then (search visited t)
              else
                let succs =  match h with
                  Node (_, Empty, Empty) -> []
                  | Node (_, lt, Empty) -> [lt]
                  | Node (_, Empty, rt) -> [rt]
                  | Node (_, lt, rt) -> [lt; rt]
                  | _ -> failwith "Matching failure!"
                in
                search (v::visited)(t @ succs)
  in search [] [rootNode];;
