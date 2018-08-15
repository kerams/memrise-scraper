open FSharp.Data
open System.IO

type Course' = HtmlProvider<"course.html">
type Pool = JsonProvider<"pool.json">
type Level' = HtmlProvider<"level.html">
type Thing = JsonProvider<"thing.json", SampleIsList = true>

let memrise = "https://www.memrise.com"

type Word = {
    TargetLanguage: string
    TargetLanguageAlts: string []
    SourceLanguage: string
    SourceLanguageAlts: string []
    Audio: string []
    Attributes: (string * string) []
    PoolId: int }

type Level = {
    Name: string
    Words: Word [] }

type Course = {
    Id: string
    TargetLanguage: string
    SourceLanguage: string
    Levels: Level []
    AttributeHeaders: (string * string) [] }

let s (str: string) = str.Replace (';', '|')

let a = Array.map s >> String.concat " / "

let createCsvHeader course =
    [ yield "Level"; yield course.TargetLanguage; yield course.TargetLanguage + " Alts"
      yield course.SourceLanguage; yield course.SourceLanguage + " Alts"; yield "Audio";
      yield! course.AttributeHeaders |> Array.map (fun (_, v) -> v + " Attribute") ]
    |> String.concat ";"

let createCsvRow attributeHeaders levelName (word: Word) =
    [ yield s levelName; yield s word.TargetLanguage; yield a word.TargetLanguageAlts
      yield s word.SourceLanguage; yield a word.SourceLanguageAlts; yield a word.Audio
      yield! attributeHeaders |> Array.map (fun (k, _) -> defaultArg (word.Attributes |> Array.tryFind (fun (k', _) -> k = k') |> Option.map snd) "") ]
    |> String.concat ";"

let dumpCsv course =
    use f = new StreamWriter (System.DateTime.Now.ToString "yyyy-MM-dd" |> sprintf "%s_%s.csv" course.Id)
    createCsvHeader course |> f.WriteLine

    for l in course.Levels do
        for w in l.Words do
             createCsvRow course.AttributeHeaders l.Name w |> f.WriteLine

let constructCourse courseId (levels: Level []) =
    let poolId = levels.[0].Words.[0].PoolId

    let pool = (sprintf "%s/api/pool/get/?pool_id=%d" memrise poolId |> Pool.Load).Pool

    let attributeHeaders = [|
        for (header, a) in pool.Attributes.JsonValue.Properties () do
           yield header, (a.GetProperty "label").AsString () |]

    { Id = courseId; AttributeHeaders = attributeHeaders; Levels = levels; TargetLanguage = pool.Columns.``1``.Label; SourceLanguage = pool.Columns.``2``.Label }

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
            level.Html.CssSelect "div[data-thing-id]"
            |> List.map (fun w ->
                let word = sprintf "%s/api/thing/get/?thing_id=%s" memrise (w.AttributeValue "data-thing-id") |> Thing.Load
                
                { TargetLanguage = defaultArg (word.Thing.Columns.``1`` |> Option.map (fun x -> x.Val)) ""
                  TargetLanguageAlts = defaultArg (word.Thing.Columns.``1`` |> Option.map (fun x -> x.Alts |> Array.map (fun y -> y.Val))) [||]
                  SourceLanguage = defaultArg (word.Thing.Columns.``2`` |> Option.map (fun x -> x.Val)) ""
                  SourceLanguageAlts = defaultArg (word.Thing.Columns.``2`` |> Option.map (fun x -> x.Alts |> Array.map (fun y -> y.Val))) [||]
                  Audio = match word.Thing.Columns.``3`` with None -> [||] | Some col -> col.Val |> Array.map (fun x -> x.Url)
                  Attributes = word.Thing.Attributes.JsonValue.Properties () |> Array.map (fun (k, v) -> k, (v.GetProperty "val").AsString ())
                  PoolId = word.Thing.PoolId })
            |> List.toArray
                        
        { Name = name; Words = words })
    |> constructCourse courseId
    |> dumpCsv

    printfn ""
    0