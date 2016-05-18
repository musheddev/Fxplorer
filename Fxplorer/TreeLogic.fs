module TreeLogic

open Types

//helper functions
let self k tree =
    tree |> Array.find (fun x -> k = x.key)

let ancestors k tree =
    let rec ann k2 tree2 nodes = //exluding root
        let x = (self k2 tree2)
        match x.ntype with 
        | TRoot -> nodes
        | _ -> ann x.data.parent tree2 (x::nodes)
    ann k tree [] |> List.toArray 

let children k tree =
    tree |> Array.filter (fun x -> x.data.parent = k)

let parent k tree =
    let p = k.data.parent
    tree |> Array.find (fun x -> x.key = p)

let leafs2 tree =
    tree |> Array.filter (fun x -> match x.ntype with
                                    | TRoot -> false
                                    | TLeaf -> true
                                    | TBranch -> false)

let rec leafs k tree =
    Array.collect (fun x -> match x.ntype with 
                            | TLeaf -> Array.singleton x
                            | TBranch -> leafs k tree
                            | TRoot -> Array.empty ) (children k tree)



let root tree =
    tree |> Array.find (fun x -> match x.ntype with
                                    | TRoot-> true
                                    | TLeaf -> false
                                    | TBranch -> false)

let depth k tree =
    let l1 = (ancestors k tree).Length
    let l2 = (children k tree) |> Array.fold (fun max x -> greater (ancestors k tree).Length max) 0
    (l1+1,l2+1)

let size t =
     let lfs = (leafs2 t)
     (Array.fold (fun acc x -> greater x.data.size  acc ) 0.0 lfs) + (Array.head lfs).data.distance

let size_ext n = n.extsize


//calculate sizes and realtive scaling factor
let calc_sizes p tree =
    tree |> Array.map (fun x -> 
        let multi = match x.ntype with
                    | TRoot -> 1.0
                    | TLeaf -> p.parent_to_children_scaling
                    | TBranch -> let depth = depth x.key tree
                                 p.parent_to_children_scaling * (1.0 + (float (fst depth))/(float (snd depth)))
        {x with relscale=multi; data={x.data with size=x.data.size*multi}})

//populate angular resoultion
let calc_ar p tree =
    let metric k = Array.sumBy (fun x -> x.data.size) (leafs k tree)
    let setar n (ar  :float<rad>)= {n with data={n.data with ar=ar}}
    let rec arinternal k (mp :float) (arp :float<rad>) = 
        Array.collect (fun x -> let mx = (metric x.key)
                                let arx = (arp/mp * mx)
                                [|(setar x arx)|] |+| (arinternal x.key mx arx)) (children k tree) 
    let root = (root tree)
    [|(setar root tau)|] |+| (arinternal root.key (metric root.key) tau )


//populate angles
let calc_theta p tree =
    let rec ctheta k theata =
        (children k tree) |> p.sort 
                          |> Array.mapFold (fun th x -> ({x with data={x.data with angle=th+0.5*x.data.ar}},th+x.data.ar) ) theata
                          |> fst
                          |> Array.collect (fun x -> match x.ntype with
                                                        | TBranch -> ctheta x.key (x.data.angle - 0.5*x.data.ar)
                                                        | _-> Array.singleton x ) 
    ctheta (root tree).key 0.0<rad> |+| Array.singleton (root tree)

//populate distances
let calc_d p tree =
    let size_r = (root tree).data.size
    let qx q = cos(q.data.angle)*q.data.distance
    let qy q = sin(q.data.angle)*q.data.distance
    let cd n t = 
        let d1 = n.data.size + size_r
        let d2 = n.data.size / (sin(n.data.ar / 2.0))
        let d3 = (parent n t).data.distance
        let d4 = (ancestors n.key t) |> Array.map (fun x -> (qx x)*cos(n.data.angle) + 
                                                               (qy x)*sin(n.data.angle) +
                                                               sqrt( sqr(n.data.size+x.data.size) - sqr((qx x)*sin(n.data.angle) - (qy x)*cos(n.data.angle)) ))
        ([|d1;d2;d3|] |+| d4) |> Array.fold greater 0.0

    let rec calc_min k t = //error
        let tre = t |> Array.map (fun x -> if x.data.parent = k then {x with data={x.data with distance=cd x t}} else x)
        (children k tre) |> Array.collect (fun x -> match x.ntype with
                                                      | TBranch -> calc_min x.key t
                                                      | _ -> Array.singleton x)  
    let temp = tree |> calc_min (root tree).key
    let d_leaf = temp |> Array.fold (fun max x -> greater max x.data.distance) 0.0
    [|(root tree)|] |+| temp |> Array.map (fun x -> match x.ntype with
                                | TLeaf -> {x with data={x.data with distance=d_leaf}}
                                | _ -> x )

//clean rare intersection cases
let clean_intersections p tree =
    tree

let calc_rel_position p tree =
    tree |> Array.map (fun x -> {x with data={x.data with position=(sin(x.data.angle)*x.data.distance,cos(x.data.angle)*x.data.distance)}})

let calc_tree p tree =
    tree |> calc_sizes p
         |> calc_ar p
         |> calc_theta p
         |> calc_d p
         |> clean_intersections p
         |> calc_rel_position p

let rec dfs_calc_tree p tree  =
    tree |> Array.map (fun x -> 
        match x.element with
        | Tree(t) ->  let t3 = dfs_calc_tree p t 
                      {x with element=Tree(t3); extsize=size t3}
        | _ -> x ) |> calc_tree p



let rec map_absolute_pos_scale tree (x,y,scale) =
    tree |> Array.map (fun n -> 
        let (acc) = (x+(fst n.data.position),y+(snd n.data.position),scale+n.relscale)
        match n.element with
        | Tree(t) ->  RTree((map_absolute_pos_scale t acc),acc,n.key)
        | Folder(d) -> RFolder(d,acc,n.key)
        | TextBox(f) -> RTextBox(f,acc,n.key))

        
let map_pos tree =
    map_absolute_pos_scale tree (0.0,0.0,1.0)