module Types

open System
open System.IO
open Gjallarhorn


[<Measure>]
type rad 

[<Literal>]
let tau=6.283185307179586476925286766559<rad>

let inline (|+|) a b = Array.append a b

let inline greater a b = if a > b then a else b

let inline sqr a = a * a

let inline sin (x :float<rad>)  = (x*(360.0/tau)) |> float |> sin 

let inline cos (x :float<rad>)  = (x*(360.0/tau)) |> float |> cos 

type NodeType =
    | TRoot
    | TLeaf
    | TBranch

type NodeData = {position : float*float
                 size : float
                 distance : float
                 ar :float<rad>
                 angle :float<rad>
                 parent :int
                 }

and Tree = Node array

and Element =
    | Tree of Tree
    | Folder of DirectoryInfo
    | TextBox of File

and Node = 
    {key :int 
     data :NodeData 
     element :Element 
     ntype :NodeType
     relscale :float
     extsize :float
     }

type Params =
    { div_tree_threshold :int 
      parent_to_children_scaling :float //0.5 is half the size
      ar_scaleing :float
      sort :Tree -> Tree}

type RenderTree =
    | RTree of (RenderTree array) * (float * float * float) * int
    | RFolder of DirectoryInfo * (float * float * float) * int
    | RTextBox of File * (float * float * float) * int