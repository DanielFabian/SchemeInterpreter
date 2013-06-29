module SchemeInterpreter.Test.Library1
open NUnit.Framework
open FsUnit
open FsCheck.NUnit
open FsCheck

[<Test>]
let ``Test function``() =
    1 |> should equal 1
    
[<Property>]
let maxLe (x:float) y = 
    (x <= y) ==> (lazy (max  x y = y))