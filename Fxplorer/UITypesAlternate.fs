module UITypesAlternate


open Gjallarhorn
open Gjallarhorn.Bindable
open Gjallarhorn.Validation
open System.Media
open System.Windows
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Controls
open System.Windows.Input
open System.Collections.Generic
open System.IO
open System
open FSharp.Core
open FSharp.Control
open System.Threading
open FsXaml
open Types
open UITypes
open TreeLogic
 

let inline rthis x _ = x

type Engine<'m,'t>(initialst :'t,runner :('m ->'t -> 't)) =
    let mbox = new MailboxProcessor<'m>(fun m -> 
        async {
           let state = ref initialst
           while true do
            let! msg = m.Receive()
            state := runner msg !state })

    member x.Start() = mbox.Start()

    member x.Post(msg) = mbox.Post(msg) 

type TreePanelEvents = 
    | MUp of MouseButtonEventArgs
    | MDown of MouseButtonEventArgs * Point * float * float
    | MWheel of MouseWheelEventArgs * Point * Point * Point
    | MMove of MouseEventArgs * Point
    | Scroll of ScrollChangedEventArgs * float * float * Point * Point * float * float


type UIMsg =
    | Resize of int * float
    | AddModels of (int * ElementState) array
    | Internal of TreePanelEvents
    | Render
    | ReRender

and ElementState = IMutatable<float*float> * IMutatable<float>

and UIState = 
    {nodes :Tree;
     models :Dictionary<int,ElementState>;}

//ELEMENTS
let createFolderELM (n :Node) (dinfo :DirectoryInfo) (engine :Engine<UIMsg,UIState>)=
    let v = EBase(n.key)
    let fldr = TextBlock()
    fldr.Text <- dinfo.Name
    v.Child <- fldr
    
    //let chg = v.Size |> Observable.add (fun x -> engine.Post(Resize(n.key,x)))
    let scale = v.Scale
    let pos = v.Position

    (v,(pos,scale)) |> Array.singleton



let createTextBoxELM (n :Node) (f :File) (engine :Engine<UIMsg,UIState>) =
    let v = EBase(n.key)
    let box = RichTextBox()
    box.HorizontalAlignment <- HorizontalAlignment.Center
    box.VerticalAlignment <- VerticalAlignment.Center
    v.Child <- box
    //let chg = v.Size |> Observable.add (fun x -> engine.Post(Resize(n.key,x)))
    let scale = v.Scale
    let pos = v.Position
    (v,(pos,scale)) |> Array.singleton

    
let rec createELM (n :Node) (engine :Engine<UIMsg,UIState>) =
    match n.element with
    | Tree(t) -> t |> Array.map (fun x -> createELM x engine) |> Array.concat
    | Folder(dinfo) -> createFolderELM n dinfo engine
    | TextBox(f) -> createTextBoxELM n f engine

////tree conversions
//let rec collapse (tree :Tree) (dic :Dictionary<int,Node>) :Dictionary<int,Node>=
//    Array.iter (fun x -> match x.element with 
//                                       | Tree(t) -> collapse t dic |> ignore
//                                                    dic.Add(x.key,{x with element=Tree(Array.empty)})
//                                       | _ -> dic.Add(x.key,x)) tree
//    dic
//
//let rec hirarchy (tree :Tree) (dic :Dictionary<int,int array>) :Dictionary<int,int array> =
//    let rkey = (Array.find (fun x ->x.ntype = TRoot) tree).key
//    let lst = tree  |> Array.filter (fun x -> not (x.ntype = TRoot)) 
//                    |> Array.fold (fun acc x -> match x.element with
//                                                | Tree(t) -> hirarchy t dic |> ignore
//                                                             x.key :: acc
//                                                | _ -> x.key :: acc) List.empty
//                    |> List.toArray
//    dic.Add(rkey,lst)
//    dic

//TREE

let  CreateTreeView (tree :Tree) =
    let view = TreePanel()
    let zinvoke x = view.Dispatcher.Invoke(Action(x))
    let p = {div_tree_threshold=10; parent_to_children_scaling=0.5; ar_scaleing=1.0; sort=(fun x -> x)}
    let zoompan msg =
        match msg with
        | MUp(args) -> zinvoke (fun () -> view.EndPan() )
        | MDown(args,p,w,h) -> zinvoke (fun () -> view.StartPan p w h)
        | MWheel(args,p,cp,tp) -> zinvoke (fun () -> view.Zoom p args.Delta cp tp)
        | MMove(args,p) -> zinvoke (fun () -> view.Pan p)
        | Scroll(args,w, h,p,tp,cw,ch) -> zinvoke (fun () -> view.Scroll args.ExtentHeightChange args.ExtentWidthChange w h p tp cw ch args.ExtentWidth args.ExtentHeight)
    
    let render st = 
        for temp in (map_pos st.nodes) do
                        let (x,y,s,key) = match temp with
                                            | RTree(_,(xx,yy,ss),k) -> (xx,yy,ss,k)
                                            | RFolder(_,(xx,yy,ss),k) -> (xx,yy,ss,k)
                                            | RTextBox(_,(xx,yy,ss),k) -> (xx,yy,ss,k)
                        printfn "set post %f %f" x y
                        if st.models.ContainsKey(key) then 
                            let z = st.models.Item(key)
                            Mutable.set (fst z) (x,y)
                            Mutable.set (snd z) (s)
        st

    let active msg (state :UIState) =
        match msg with
        | AddModels(mdls) -> {state with models=(mdls |> Array.fold (fun dic x -> dic.Add(fst x, snd x) |> rthis dic ) (Dictionary<int,ElementState>()))}
        | Resize(size,key) -> state
        | Internal(imsg) -> zoompan imsg
                            state
        | Render -> printfn "render" 
                    render state
        | ReRender -> printfn "rerender" 
                      { state with nodes=(dfs_calc_tree p state.nodes)} |> render

    let state = {nodes=tree;models=(Dictionary<int,ElementState>())}
    let engine = Engine<UIMsg,UIState>(state,active)
    engine.Start()
    
    let velm = tree |> Array.map (fun x -> createELM x engine) |>  Array.concat 
    for x in velm do view.canvas.Children.Add (fst x) |> ignore
    velm |> Array.map (fun x -> (fst x).Key, snd x) |> AddModels |> engine.Post

    view.MouseLeftButtonDown.Add(fun e -> engine.Post(Internal(MDown(e,e.GetPosition(view),view.ViewportWidth,view.ViewportHeight))))
    view.MouseLeftButtonUp.Add(fun e -> engine.Post(Internal(MUp(e))))
    view.MouseMove.Add(fun e -> engine.Post(Internal(MMove(e,e.GetPosition(view)))))
    view.ScrollChanged.Add(fun e -> engine.Post(Internal(Scroll(e,view.ViewportWidth,view.ViewportHeight,Mouse.GetPosition(view.canvas),
                                                                view.TranslatePoint(Point(0.5*view.ViewportWidth,0.5*view.ViewportHeight),view.canvas),
                                                                view.canvas.Width,view.canvas.Height))))
    view.MouseWheel.Add(fun e -> engine.Post(Internal(MWheel(e,e.GetPosition(view),e.GetPosition(view.canvas),
                                                                     view.TranslatePoint(Point(0.5*view.ViewportWidth,0.5*view.ViewportHeight),view.canvas)))))
    engine.Post(ReRender)
    view

//WINDOW
let createWindow () =
    let win = new MainWindow()
    let orgin = [| {key=0; data={position=0.0,0.0;size=1.0;distance=0.0;ar=0.0<rad>;angle=0.0<rad>;parent= -1;}; ntype=TRoot; relscale=1.0; element=Folder (DirectoryInfo "C:"); extsize=0.0} |] 
    let tree = CreateTreeView orgin
    win.treeviewcontainer.Content <- tree
    win