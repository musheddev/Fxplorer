module UITypes

open Gjallarhorn

open System
open System.Threading
open System.Windows
open System.Windows.Controls
open System.Media
open System.Windows
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Controls
open System.Windows.Input
open System.Collections.Generic
open System.Reactive.Concurrency
open System.Reactive
open System.Reactive.Linq


open Types

type EngineContext =
    {tree :IMutatable<Tree>
     sizes :Dictionary<int,IMutatable<float>>
     }


type MainWindow = FsXaml.XAML<"xaml/FxplorerWindow.xaml", true>

type FolderCtl = FsXaml.XAML<"xaml/FolderControl.xaml",true>

type EBase(key) as this =
    inherit Border()
    let _key :int = key
    let _size = Mutable.create 0.0
    let _scale = Mutable.create 0.0
    let _pos = Mutable.create (0.0,0.0)
    do base.CornerRadius <- CornerRadius(100.0)
    do base.BorderBrush <- Brushes.AliceBlue
    do base.BorderThickness <- Windows.Thickness(1.0)
    let stran = ScaleTransform()
    do base.LayoutTransform <- stran
    //let ctx = Threading.SynchronizationContext.Current

//    let comp_size () = 
//        let s = this.Child.DesiredSize
//        let s2 = (0.5 * Math.Sqrt(s.Width*s.Width + s.Height*s.Height))
//        if not (Mutable.get _size = s2) then Mutable.set _size s2
//        s2

    let posob = _pos |> Observable.subscribe (fun x -> this.Dispatcher.Invoke(Action(fun z ->
                                                                Canvas.SetLeft(this,fst x)
                                                                Canvas.SetTop(this,snd x) )))

    let scaleob = _scale |> Observable.subscribe (fun x -> stran.Dispatcher.Invoke(Action(fun z -> stran.ScaleX <- x
                                                                                                   stran.ScaleY <- x)))

    member x.Key with get () =  _key

    
    override this.MeasureOverride(nan :Size) =
        base.Child.Measure(nan)
        let s = this.Child.DesiredSize
        let s2 = (0.5 * Math.Sqrt(s.Width*s.Width + s.Height*s.Height))
        if not (Mutable.get _size = s2) then Mutable.set _size s2
        Size(s2,s2)
        //calculate from child

   // override this.ArrangeOverride(size :Size) =
     //   base.Child.VisualTransform <- Media.TranslateTransform()
        //posiotn child in center
            //in by 0.5*(parent - child) width
            //down by ...

    member x.Size with get () = _size
    member x.Scale with get () = _scale
    member x.Position with get () = _pos


type CenteredCanvas() =
    inherit Canvas()

    let mutable cen_y = 0.0
    let mutable cen_x = 0.0

    member this.CenterY with get () = cen_y and private set (y) = cen_y <- y
    member this.CenterX with get () = cen_x and private set (x) = cen_x <- x

  //  override this.MeasureOverride(nan :Size) =
        //calcuate extenet on quad sides of center
        //calcuate center
  //      nan

    override this.ArrangeOverride(size) =
        for e in base.InternalChildren do
            let l = if Double.IsNaN (Canvas.GetLeft(e)) then 0.0 else Canvas.GetLeft(e) + cen_x

            let b = cen_y - if Double.IsNaN (Canvas.GetTop(e)) then 0.0 else Canvas.GetTop(e)
            e.Arrange( Rect(  Point( l, b), e.DesiredSize ) )
        size

type TreePanel() = 
    inherit ScrollViewer() 

    let can = CenteredCanvas()
    let stran = ScaleTransform()
    do base.LayoutTransform <- stran
    do base.HorizontalScrollBarVisibility <- ScrollBarVisibility.Visible
    do base.VerticalScrollBarVisibility <- ScrollBarVisibility.Visible
    do base.Content <- can

    let mutable lastCenterPositionOnTarget :Point option  = None
    let mutable lastMousePositionOnTarget :Point option  = None
    let mutable lastDragPoint :Point option = None

    member this.canvas with get () = can

    member this.Pan (p :Point)  =
        match lastDragPoint with
        | Some(dp) -> 
            this.ScrollToHorizontalOffset(this.HorizontalOffset - p.X + dp.X)
            this.ScrollToVerticalOffset( this.VerticalOffset - p.Y + dp.Y)
            lastDragPoint <- Some(p)
        | None -> ()


    member this.Zoom point delta childpos tp  =
        lastMousePositionOnTarget <- Some(childpos)
        let mutable scale = stran.ScaleX
        match (delta,scale) with
            | (d,s) when (d > 0) && (s >= 1.0) -> scale <- scale + 0.5
            | (d,s) when (d > 0)  -> scale <- scale + 0.1
            | (d,s) when (d < 0) && (s > 1.0) -> scale <- scale - 0.5
            | (d,s) when (d < 0) && (s > 0.1) -> scale <- scale - 0.1
            | (_,_) -> ()
        stran.ScaleX <- scale
        stran.ScaleY <- scale
        lastCenterPositionOnTarget <- Some(tp)


    member this.EndPan ()  =
        this.Cursor <- Cursors.Arrow
        lastDragPoint <- None
        this.ReleaseMouseCapture()
        


    member this.StartPan (p :Point) w h  =
        if (p.X <= w && p.Y < h) then 
            this.Cursor <- Cursors.SizeAll
            lastDragPoint <- Some(p)
            this.CaptureMouse() |> ignore

    member this.Scroll ehc ewc vpw vph pos tp cw ch ew eh =
        if not(ehc=0.0) || not(ewc=0.0) then
            let mutable oldtarget,(newtarget :Point option) = None,None
            match lastMousePositionOnTarget,lastCenterPositionOnTarget with
            | Some(mp),Some(cp) -> oldtarget <- lastCenterPositionOnTarget
                                   newtarget <- Some(tp)
            | _,_ -> newtarget <- Some(pos)
                     oldtarget <- lastMousePositionOnTarget
                     lastMousePositionOnTarget <- None
            match oldtarget,newtarget with 
            | Some(ot),Some(nt) ->  this.ScrollToHorizontalOffset(this.HorizontalOffset - (nt.X - ot.X)*(ew/cw))
                                    this.ScrollToVerticalOffset( this.VerticalOffset - (nt.Y - ot.Y)*(eh/ch))
            | _,_ -> ()

