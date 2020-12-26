namespace HccFS

module Ksexp =
    open MyUtil

    type SexpObject =
        | Float of float
        | Bool of bool
        | String of string
        | Symbol of string
        | List of SexpObject list
        | Object of SexpObject
        | Quote of SexpObject

    let sexpObjectKind =
        function
        | Float _ -> 0
        | Bool _ -> 1
        | String _ -> 2
        | Symbol _ -> 3
        | List _ -> 4
        | Object _ -> 5
        | Quote _ -> 6

    type ParseResult =
        { parseResult: SexpObject option
          readLen: int }

    let symbolChars =
        let tmpCharSet = ref Set.empty
        String.iter (fun c -> csAddRef tmpCharSet c) "~!@#$%^&*-_=+:/?<>"
        !tmpCharSet

    let nextBracket code leftOffset =
        let index = 0
        let leftCount = leftOffset
        let rightCount = 0

        let rec loop index leftCount rightCount =
            if index < String.length code
               && leftCount <> rightCount then
                if code.[index] = '('
                then loop (index + 1) (leftCount + 1) rightCount
                else if code.[index] = ')'
                then loop (index + 1) leftCount (rightCount + 1)
                else loop (index + 1) leftCount rightCount
            else
                index

        loop index leftCount rightCount

    let rec parseList (str: string) =
        let i = ref 1
        let lst = ref []
        let nextBracketIdx = nextBracket str.[1..] 1
        let content = str.[1..nextBracketIdx]

        let rec loop j =
            if j < String.length content - 1 then
                let s = content.[j..]
                let tmpResult = sexpParseExpr s
                (match tmpResult.parseResult with
                 | Some p -> listAddRef lst p
                 | None -> ())
                loop (j + tmpResult.readLen)
            else
                j

        let j = loop 0
        addRefInt i j
        assert (str.[!i] = ')')
        addRefInt i 1
        { parseResult = Some(List !lst)
          readLen = !i }

    and skipLine str =
        let strLen = String.length str

        let rec loop i =
            if i < strLen && str.[i] <> '\n' then loop (i + 1) else i

        let readLen = loop 0
        { parseResult = None
          readLen = readLen }

    and parseNumber str =
        let dotNextIsNumber (str: string) strLen i =
            (str.[i] = '.'
             && i + 1 < strLen
             && isDigit (str.[i + 1]))

        let i = ref 0
        let strLen = String.length str

        let first =
            if str.[0] = '-' then
                (addRefInt i 1
                 1)
            else
                0

        let rec loop i =
            if i < strLen
               && (isDigit (str.[i]) || dotNextIsNumber str strLen i) then
                loop (i + 1)
            else
                i

        let i = loop !i
        let tmpLen = i - first
        let tmp = str.[first..first + tmpLen - 1]

        let value =
            (float tmp) * if first = 1 then -1.0 else 1.

        { parseResult = Some(Float value)
          readLen = i }

    and parseSymbol str =
        let strLen = String.length str

        let rec loop i =
            if i < strLen
               && (isAlpha str.[i]
                   || Set.exists ((=) str.[i]) symbolChars) then
                loop (i + 1)
            else
                i

        let i = loop 0
        let tmp = str.[0..i - 1]
        { parseResult = Some(Symbol tmp)
          readLen = i }

    and parseString str =
        let strLen = String.length str

        let rec loop i =
            if i < strLen && str.[i] <> '"' then loop (i + 1) else i

        let i = loop 1
        let tmp = str.[1..i - 1]
        { parseResult = Some(String tmp)
          readLen = i + 1 }

    and parseQuote (str: string) =
        let expr = str.[1..] |> sexpParseExpr
        { parseResult =
              match expr.parseResult with
              | Some e -> Some(Quote e)
              | None -> None
          readLen = 1 + expr.readLen }

    and sexpParseExpr code =
        let codeLen = String.length code

        let rec loop i =
            if i < codeLen then
                let c = code.[i]
                match c with
                | ' '
                | '\n'
                | '\r'
                | '\t' -> loop (i + 1)
                | ';' ->
                    let j =
                        let t = skipLine code.[i..]
                        t.readLen

                    loop (i + j)
                | c when isDigit c
                         || (c = '-' && i + 1 < codeLen && isDigit code.[i + 1]) ->
                    let result = parseNumber code.[i..]
                    { parseResult = result.parseResult
                      readLen = i + result.readLen }
                | c when isAlpha c || Set.exists ((=) c) symbolChars ->
                    let result = parseSymbol code.[i..]
                    { parseResult = result.parseResult
                      readLen = i + result.readLen }
                | c when c = '"' ->
                    let result = parseString code.[i..]
                    { parseResult = result.parseResult
                      readLen = i + result.readLen }
                | c when c = '(' ->
                    let result = parseList code.[i..]
                    { parseResult = result.parseResult
                      readLen = i + result.readLen }
                | c when c = ''' ->
                    let result = parseQuote code.[i..]
                    { parseResult = result.parseResult
                      readLen = i + result.readLen }
                | _ -> failwith "invalid"
            else
                { parseResult = None; readLen = i }

        loop 0

    let sexpParse code =
        let codeLen = String.length code

        let rec loop i =
            if i < codeLen then
                let s = code.[i..]
                let result = sexpParseExpr s
                match result.parseResult with
                | Some v -> v :: loop (i + result.readLen)
                | None -> []
            else
                []

        loop 0
