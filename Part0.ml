type 'a bt = 
  | Leaf 
  | Node of 'a * 'a bt * 'a bt

  (* Binary Tree definition *)
  let bt = 
    Node (1, 
          Node(2, 
              Node(4, Leaf, Leaf), 
              Node(5, Leaf, Leaf)),
          Node(3, 
              Node(6, Leaf, Leaf), 
              Node(7, Leaf, Leaf)));;
  let bt1 = 
    Node (9, 
          Node(2, 
              Node(4, Leaf, Leaf), 
              Node(5, Leaf, Leaf)),
          Node(3, 
              Leaf, 
              Leaf));;
let bt2 = 
    Node (1, 
          Node(2, 
              Node(4, Leaf, Leaf), 
              Node(5, Leaf, Leaf)),
          Node(3, 
              Node(6, Leaf, Leaf), 
              Node(7, Leaf, Leaf)));;

    (** does the tree contain an element equal to the given one? *)

let rec contains x = function
| Leaf -> false
| Node (v, l, r) -> v = x || contains x l || contains x r;;

    (** produce elements in the left-to-right order they appear in the tree *)

let rec print_t = function
    | Leaf -> []
    | Node (v, l, r) -> [v] @ print_t l @ print_t r;;

    (* Zip together two binary trees with the same shape; fail if not same shape *)

let rec is_same a b = 
    match a, b with
    | Leaf, Leaf -> true
    | Node (_, l1, r1), Node(_, l2, r2) -> is_same l1 r2 && is_same r1 l2
    | _ -> false;;
let rec zip a b = 
    match is_same a b with 
    | true-> begin
        match a with
          |Node(v1, l1, r1)->begin
            match b with
            |Node(v2, l2, r2)-> Node((v1,v2), zip l1 l2, zip r1 r2)
            |Leaf-> Leaf
          end
        | Leaf-> Leaf
        end
    | false -> failwith "Trees not of the same shape!";;

    (* Produce tree of smae shape, every element is obtained by applying f *)

let rec map a f =  
    match a with
    |Leaf -> Leaf
    |Node(v, l, r) -> Node(f v, map  l f ,  map r f );;

    (* Produce the mirror image of a tree; elements are in reverse order *)

let rec rev = function
    | Leaf -> Leaf
    | Node(v, l, r) -> Node(v, rev r, rev l);;


    (* list List.fold but for binary trees *)

type dir = Leaf | Right

    (* get the element found by following given path *)

    (* let get_element = function
    Leaf -> failwith "fail"
    | Node(data, left, right)-> data
    
    let rec get a b= match a with
    Leaf -> failwith "fail"
    |Node(data, left, right)->begin 
      match b with
      [] ->failwith "fail"
      | head::tail -> begin
        match head with 
        Left-> begin
          match tail with
          |[]->get_element left
          |h::t-> get left tail
        end
        | Right ->begin
          match tail with
          |[]->get_element left
          |h::t-> get right tail
        end 
      end
    end *)



    (* Binary Search Tree *)

    type 'a bst = 
        | Leaf
        | Node of 'a bst * 'a * 'a bst;;
        
        let bst = 
            Node(Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)), 4, Node(Node(Leaf, 5, Leaf), 6, Node(Leaf, 8, Leaf)));;
            
            (** insert the given element into bst *)

let rec bst_insert x = function
    | Leaf -> Node(Leaf, x, Leaf)
    | Node(l, v, r) -> 
        if x < v then Node (bst_insert x l, v, r)
        else Node(l, v, bst_insert x r);;

        (* Does the bst containes an element equals to the given one ? *)
    
let rec bst_contains x = function
    | Node(l, v, r) -> 
        if x = v then true 
        else if x < v then bst_contains x l 
        else bst_contains x r
    | Leaf -> false;;

    (** delete the first occurrence of given element; fail if it doesn't exist *)

let rec bst_delete x = function
    | Leaf -> failwith "Fail!"
    | Node(l, v, r) -> 
    if x = v 