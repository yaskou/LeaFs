open LeaFs

[<EntryPoint>]
let main args =
    args
    |> Seq.iter (fun arg ->
        arg
        |> convertHiragana
        |> calculateAffinity
        |> printfn "%s%%"
    )
    0
