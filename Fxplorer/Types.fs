module Types


type NodeType =
    | TRoot
    | TLeaf
    | TBranch

type NodeData = {position : Tree -> float*float
                 size : float
                 distance : float
                 ar :Tree -> float
                 angle :Tree -> (Tree -> Tree)-> float
                 key :int
                 parent :int
                 ntype :NodeType
                 }

and Tree = Node array

and Leaf =
    | Tree of Tree * float
    | Folder
    | TextBox
and Node =
    | Root of int * NodeData
    | Leaf of int * NodeData *Leaf
    | Branch of int * NodeData


let nodedata n =
    match n with
    | Root(_,nd) -> nd
    | Leaf(_,nd,_) -> nd
    | Branch(_,nd) -> nd

let key n =
    match n with
    | Root(i,_) -> i
    | Leaf(i,_,_) -> i
    | Branch(i,_) -> i

let self k tree =
    tree |> Array.find (fun x -> k = key x)

let ancestors k tree =
    let rec ann k2 tree2 nodes = //exluding root
        let x = (self k2 tree2)
        let d = nodedata x
        match x with 
        | Root(_,_) -> nodes
        | _ -> ann d.parent tree2 (x::nodes)
    ann k tree [] |> List.toArray 

let children k tree =
    tree |> Array.filter (fun x -> (nodedata x).parent = k)

let parent k tree =
    let p = (nodedata k).parent
    tree |> Array.find (fun x -> key x = p)

let leafs tree =
    tree |> Array.filter (fun x -> match x with
                                    | Root(_,_) -> false
                                    | Leaf(_,_,_) -> true
                                    | Branch(_,_) -> false)


let root k tree =
    tree |> Array.find (fun x -> match x with
                                    | Root(_,_) -> true
                                    | Leaf(_,_,_) -> false
                                    | Branch(_,_) -> false)



let size l =
    match l with
    | Tree(t,s) ->  let lfs = (leafs t)
                    (Array.fold (fun acc x -> if (nodedata x).size > acc then (nodedata x).size  else acc ) 0.0 lfs) + (lfs |> Array.head |> nodedata ).distance
    | Folder -> 1.0
    | TextBox -> 1.0


