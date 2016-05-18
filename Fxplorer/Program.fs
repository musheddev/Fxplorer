// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open UITypesAlternate
open System
open System.Windows




[<EntryPoint>][<STAThread>]
let main argv = 
    let window = createWindow()
    (new Application()).Run(window.Root) 