module LeaFs

let convertHiragana (name: string) =
    name
    |> Seq.toList
    |> List.map (fun character ->
        match character with
        | character when "あかさたなはまやらわがざだばぱゃ".Contains character -> 1
        | character when "いきしちにひみりぎじぢびぴ".Contains character -> 2
        | character when "うくすつぬふむゆるをぐずづっぶぷゅ".Contains character -> 3
        | character when "えけせてねへめれげぜでべぺ".Contains character -> 4
        | character when "おこそとのほもよろんごぞどぼぽょ".Contains character -> 5
        | _ -> 0
    )
    |> List.filter (fun i -> i <> 0)
