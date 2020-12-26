namespace HccFS

module MyUtil =
    type StringSet = Set<string>

    type CharSet = Set<char>

    let refOp op r = op !r

    let refOp2 op r v = op !r v

    let effectOpRef op r = r := refOp op r

    let effectOp2Ref op r v = r := refOp2 op r v

    let ssAddRef ss value = ss := Set.add value !ss

    let csAddRef cs value = cs := Set.add value !cs

    let listAddRef lst value = lst := List.append !lst [ value ]

    let listAppendRef lst lst2 = lst := List.append !lst lst2

    let addRefInt i v = i := !i + v

    let (>>) f g x = g (f x)

    let rec range i j =
        if i < j then i :: range (i + 1) j else []

    let isDigit =
        function
        | e when '0' <= e && e <= '9' -> true
        | _ -> false

    let isAlpha =
        function
        | e when 'a' <= e && e <= 'z' || 'A' <= e && e <= 'Z' -> true
        | _ -> false

    let intOfStringOpt (x: string) =
        try
            let y = int x in Some(y)
        with :? System.FormatException -> None

    let floatOfStringOpt (x: string) =
        try
            let y = float x in Some(y)
        with :? System.FormatException -> None
