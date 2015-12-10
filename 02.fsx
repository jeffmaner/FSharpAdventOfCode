// Day02 is my initial, functional approach to the solution.
// Day02C is my more object-oriented approach to the solution. I think I like
// it better in this instance.
module Day02 =
  open System.IO

  type Surface = { length : int; width : int; area : int; perimeter : int }
  let makeSurface l w = { length = l; width = w; area = l * w; perimeter = 2 * (l + w) }

  type Present = { face : Surface; top : Surface; side : Surface }
  let makePresent l w h =
      { face = makeSurface l w; top = makeSurface w h; side = makeSurface h l }

  let inline map xs = Seq.map xs
  let inline sum xs = Seq.sum xs

  let (</>) p q = Path.Combine (p, q)

  let presents =
    __SOURCE_DIRECTORY__ </> "02.input"
    |> File.ReadAllLines
    |> map (fun s -> s.Split [| 'x' |])
    |> map (map int >> Seq.toList)
    |> map (function
                  | [ l; w; h ] -> makePresent l w h
                  | _           -> failwith "Undefined")

  // Part One.
  let totalWrappingPaperSquareFeet (ps:Present seq) =
      let surfaceArea (p:Present) = 2 * (p.face.area + p.top.area + p.side.area)
      let areaOfSmallestSide (p:Present) = List.min [p.face.area; p.top.area; p.side.area]
      let presentRequirement p = surfaceArea p + areaOfSmallestSide p

       in (map presentRequirement >> sum) ps

  totalWrappingPaperSquareFeet presents // 1588178

  // Part Two.
  let totalRibbonFeet (ps:Present seq) =
      let smallestPerimeter (p:Present) = List.min [p.face.perimeter; p.top.perimeter; p.side.perimeter]
      let volume (p:Present) = p.face.length * p.top.length * p.side.length
      let presentRequirement p = smallestPerimeter p + volume p

       in (map presentRequirement >> sum) ps

  totalRibbonFeet presents // 3783758

module Day02C =
  open System.IO

  type Surface (l, w) =
      member s.Area = l * w
      member s.Perimeter = 2 * (l + w)

  type Present (l, w, h) =
      let face = new Surface(l, w)
      let top  = new Surface(w, h)
      let side = new Surface(h, l)
      member p.SurfaceArea = 2 * (face.Area + top.Area + side.Area)
      member p.Volume = l * w * h
      member p.SmallestSurface = [face; top; side] |> List.minBy (fun s -> s.Area)
      member p.RequiredWrappingPaperSquareFeet = p.SurfaceArea + p.SmallestSurface.Area
      member p.RequiredRibbonFeet = p.SmallestSurface.Perimeter + p.Volume

  let inline map f xs = Seq.map f xs
  let inline sum   xs = Seq.sum   xs

  let (</>) p q = Path.Combine (p, q)

  let presents =
    __SOURCE_DIRECTORY__ </> "02.input"
    |> File.ReadAllLines
    |> map (fun s -> s.Split [| 'x' |])
    |> map (map int >> Seq.toList)
    |> map (function
              | [ l; w; h ] -> new Present(l, w, h)
              | _           -> failwith "Undefined")

  presents |> (map (fun p -> p.RequiredWrappingPaperSquareFeet) >> sum) // 1588178
  presents |> (map (fun p -> p.RequiredRibbonFeet) >> sum) // 3783758

// vim:ft=fs