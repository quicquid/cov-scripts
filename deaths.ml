module Point = struct
  type t = { year : int; week : int }
  let compare a b = compare (a.year, a.week) (b.year, b.week)
  let point year week = { year; week }
  let year { year; _ } = year
  let week { week; _ } = week

  let pp ?sep:(sep=", ") fmt {year; week} = CCFormat.fprintf fmt "%d%s%d" year sep week
end

module PointMap = CCMap.Make(Point)
module IntMap = CCMap.Make(CCInt)
module IntSet = CCSet.Make(CCInt)

let parse_point str =
  CCString.chop_prefix ~pre:"KALW-" str |>
  CCOpt.map (fun x ->
      let year = Str.string_before x 4|> int_of_string in
      let week = Str.string_after x 4|> int_of_string in
      Point.point year week
    )

let lines = Csv.load "OGD_gest_kalwo_GEST_KALWOCHE_100.csv"

let parsed = lines |> CCList.map (function
    | pt::_::_::_::deaths::_ ->
      parse_point pt |> CCOpt.map (fun x -> (x,int_of_string deaths))
    | _ -> None
  )

let data = CCList.fold_left (fun acc -> function
    | Some (point, deaths) ->
      let oldval = PointMap.get_or point acc ~default:0 in
      PointMap.add point (oldval + deaths)  acc
    | None -> acc
  ) PointMap.empty parsed

let by_year = PointMap.fold (function {Point.year; week } -> fun deaths acc ->
    IntMap.update year (function | Some v -> Some (v + deaths) | None -> Some deaths) acc
  ) data IntMap.empty

let by_year_list =
  by_year |> IntMap.to_list |>
  CCList.sort (fun x y -> compare (fst x) (fst y))

let by_months_list =
  data |> PointMap.to_list |>
  CCList.sort (fun x y -> Point.compare (fst x) (fst y))  

let print_by_month () =
  by_months_list |>
  CCFormat.printf "%a@." ( (CCList.pp ~pp_sep:(fun fmt () -> CCFormat.fprintf fmt "@,")
                             (CCPair.pp Point.pp CCInt.pp |> CCFormat.hbox)) |> CCFormat.vbox)


let print_by_year () =
  by_year_list |>
  CCFormat.printf "%a@." ((CCList.pp ~pp_sep:(fun fmt () -> CCFormat.fprintf fmt "@,")
                             (CCPair.pp CCInt.pp CCInt.pp |> CCFormat.hbox)) |> CCFormat.vbox )

module Gp = Gnuplot

let gp = Gp.create ()

let plot gp =
  let _years =
    CCList.map (function (x,y) -> (float_of_int x, float_of_int y)) by_year_list |>
    Gp.Series.lines_xy
  in
  let allyears =
    CCList.map fst by_year_list |> IntSet.of_list |> IntSet.to_list |> CCList.sort compare
  in
  let weeks =
    CCList.map (fun y ->
        let scaled = (y-2000)*10 in
        let (style, color) = match y with
          | 2020 -> (Gp.Series.lines_xy, `Rgb (200,30,50))
          (*          | _ -> `Rgb (scaled*3/4, 220-scaled, scaled) *)
          | _ -> (Gp.Series.points_xy, `Rgb (50,50,50))
        in
        CCList.filter (function ({Point.year; _}, _) ->  y = year) by_months_list |>
        CCList.map (function ({Point.week; _}, deaths ) -> (float_of_int week, float_of_int deaths)) |>
        style ~title:(string_of_int y) ~color
      ) allyears
  in
  (* Gp.plot_many gp [years] *)
  Gp.plot_many gp weeks

let () =
  plot gp
