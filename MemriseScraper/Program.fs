open FSharp.Data
open System.IO

type Course' = HtmlProvider<"course.html">
type Pool = JsonProvider<"pool.json">
type Level' = HtmlProvider<"level.html">
type Thing = JsonProvider<"thing.json", SampleIsList = true>

let memrise = "https://app.memrise.com"

type ColumnId = ColumnId of string

type ColumnHeader = {
    Id: ColumnId
    HasAlts: bool
    Name: string }

type ColumnValue =
    | TextWithAlts of string * string list
    | Text of string
    | Audio of string list
    | Image of string list
    | Video of string list

type Word = {
    PoolId: int
    Columns: Map<ColumnId, ColumnValue> }

type Level = {
    Name: string
    Words: Word list }

type Course = {
    Id: string
    Levels: Level list
    ColumnHeaders: ColumnHeader list }

let escapeTextForCsvCell (str: string) =
    let needToQuote = str |> String.exists (fun c -> c = ';' || c = '"' || c = '\r' || c = '\n')

    if needToQuote then
        let str = str.Replace ("\"", "\"\"")
        "\"" + str + "\""
    else
        str

let concatAlts = String.concat " / " >> escapeTextForCsvCell

let unifyColumnsAndAttributes (thing: Thing.Thing) =
    let cols =
        thing.Columns.JsonValue.Properties ()
        |> Array.map (fun (k, v) ->
            match (v.GetProperty "kind").AsString () with
            | "text" ->
                ColumnId k, TextWithAlts ((v.GetProperty "val").AsString (), (v.GetProperty "alts").AsArray () |> Array.map (fun v -> (v.GetProperty "val").AsString ()) |> Array.toList)
            | "image" ->
                ColumnId k, Image ((v.GetProperty "val").AsArray () |> Array.map (fun v -> (v.GetProperty "url").AsString ()) |> Array.toList)
            | "audio" ->
                ColumnId k, Audio ((v.GetProperty "val").AsArray () |> Array.map (fun v -> (v.GetProperty "url").AsString ()) |> Array.toList)
            | "video" ->
                ColumnId k, Video ((v.GetProperty "val").AsArray () |> Array.map (fun v -> (v.GetProperty "high").AsString ()) |> Array.toList)
            | _ -> failwith "Unexpected column kind")

    let atts =
        thing.Attributes.JsonValue.Properties ()
        |> Array.map (fun (k, v) -> ColumnId ("a" + k), Text ((v.GetProperty "val").AsString ()))

    Array.append cols atts |> Map.ofArray

let createCsvHeader course =
    "Level" :: (course.ColumnHeaders |> List.collect (fun { Name = name; HasAlts = hasAlts } -> if hasAlts then [ escapeTextForCsvCell name; escapeTextForCsvCell (name + " alts") ] else [ escapeTextForCsvCell name ]))
    |> String.concat ";"

let createCsvRow headers levelName word =
    let columnString col =
        match col with
        | TextWithAlts (main, alts) -> [ escapeTextForCsvCell main; concatAlts alts ]
        | Text text -> [ escapeTextForCsvCell text ]
        | Image urls -> [ concatAlts urls ]
        | Audio urls -> [ concatAlts urls ]
        | Video urls -> [ concatAlts urls ]

    let mainCols =
        headers |> List.collect (fun { Id = id; HasAlts = hasAlts } -> defaultArg (Map.tryFind id word.Columns |> Option.map columnString) (if hasAlts then [ ""; "" ] else [ "" ]))

    escapeTextForCsvCell levelName :: mainCols
    |> String.concat ";"

let dumpCsv course =
    match course with
    | None ->
        printfn "\nCourses with a single level are not supported."
    | Some course ->
        let fileName = System.DateTime.Now.ToString "yyyy-MM-dd" |> sprintf "%s_%s.csv" course.Id
        use f = new StreamWriter (fileName)
        createCsvHeader course |> f.WriteLine

        for l in course.Levels do
            for w in l.Words do
                 createCsvRow course.ColumnHeaders l.Name w |> f.WriteLine

        printfn "\nCreated %s." fileName

let constructCourse courseId levels =
    match levels |> List.tryPick (fun l -> List.tryHead l.Words |> Option.map (fun w -> w.PoolId)) with
    | None ->
        None
    | Some poolId ->
        let pool = (sprintf "%s/api/pool/get/?pool_id=%d" memrise poolId |> Pool.Load).Pool

        let headers =
            let cols1 = pool.Columns.JsonValue.Properties () |> Array.map (fun (v, a) -> { Name = (a.GetProperty "label").AsString (); Id = ColumnId v; HasAlts = (a.GetProperty "kind").AsString () = "text" })
            let cols2 = pool.Attributes.JsonValue.Properties () |> Array.map (fun (v, a) -> { Name = (a.GetProperty "label").AsString (); Id = ColumnId ("a" + v); HasAlts = false })
            Array.append cols1 cols2 |> Array.toList
        
        Some { Id = courseId; ColumnHeaders = headers; Levels = levels }

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then
        printfn "Provide a course ID."
        exit 1

    let courseId = argv.[0]
    let course = sprintf "%s/course/%s" memrise courseId |> Course'.Load

    course.Html.CssSelect "a.level"
    |> List.map (fun a -> sprintf "%s%s" memrise (a.AttributeValue "href"))
    |> List.toArray
    |> Array.Parallel.map (fun level ->
        printf "."
        let level = Level'.Load level
        let name = (level.Html.CssSelect "h3.progress-box-title" |> List.head).InnerText().Trim()

        let words =
            if level.Html.CssSelect ".grammar-download" |> Seq.isEmpty then
                level.Html.CssSelect "div[data-thing-id]"
                |> List.map (fun w ->
                    let thing = sprintf "%s/api/thing/get/?thing_id=%s" memrise (w.AttributeValue "data-thing-id") |> Thing.Load
                    { Columns = unifyColumnsAndAttributes thing.Thing; PoolId = thing.Thing.PoolId })
            else
                []
                        
        { Name = name; Words = words })
    |> Array.toList
    |> constructCourse courseId
    |> dumpCsv

    0