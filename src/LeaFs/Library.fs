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

let calculateAffinity (numbers: list<int>) =
    let rec factorial n =
        match n with
        | 0 | 1 -> 1
        | _ -> factorial (n - 1) * n
    let binomialTheorem n r =
        factorial n / (factorial r * factorial (n - r))

    ("", [ numbers[..numbers.Length - 2]; numbers.Tail ])
    ||> List.fold (fun acc numbers ->
        acc + (numbers
        |> List.reduce (fun prev i ->
            prev + binomialTheorem (numbers.Length - 1) (List.findIndex ((=) i) numbers) * i
        )
        |> string |> Seq.last |> string)
    )
