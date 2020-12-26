namespace HccFS

module Compiler =
    open Ksexp
    open MyUtil
    open OpCode

    let VMValueKind =
        function
        | VValue _ -> 0
        | VFunc _ -> 1

    let OpCodeKind =
        function
        | OpPop -> 0
        | OpPush _ -> 1
        | OpAllocLvars _ -> 2
        | OpFreeVars -> 3
        | OpGetLocal _ -> 4
        | OpSetLocal _ -> 5
        | OpSetArgLocal _ -> 6
        | OpAdd -> 7
        | OpSub -> 8
        | OpMul -> 9
        | OpDiv -> 10
        | OpMod -> 11
        | OpEq -> 12
        | OpNeq -> 13
        | OpLt -> 14
        | OpLeq -> 15
        | OpGt -> 16
        | OpGeq -> 17
        | OpPrint -> 18
        | OpPrintln -> 19
        | OpJumpRel _ -> 20
        | OpFuncDef _ -> 21
        | OpCall _ -> 22
        | OpReturn -> 23
        | OpVarDef _ -> 24
        | OpGetVar _ -> 25
        | OpSetVar _ -> 26
        | OpBranch _ -> 27
        | OpMakeList _ -> 28
        | OpSetArgFrom _ -> 29
        | OpDumpEnv -> 30

    let OpCodeWidth (_: OpCode) = 1

    let OpCodesLen = List.map OpCodeWidth >> List.fold (+) 0

    let isNumber t =
        let t1 = intOfStringOpt t
        let t2 = floatOfStringOpt t
        match (t1, t2) with
        | (None, None) -> false
        | _ -> true

    let isString t =
        if String.length t < 2 then
            false
        else
            let c1 = t.[0]
            let c2 = t.[String.length t - 1]
            c1 = '\"' && c1 = c2

    let failInvalid msg =
        failwith
            (match msg with
             | None -> "invalid source code given"
             | Some t -> Printf.sprintf "invalid source code given - %s" t)

    let compileDebug = false

    let rec compileSexpObject sexp =
        if compileDebug then printfn "sexp: %A" sexp
        match sexp with
        | Float _
        | Bool _
        | String _ -> [ OpPush(VValue sexp) ]
        | Symbol sym -> [ OpGetVar sym ]
        | List (Symbol x :: xs) when x = "def-var" ->
            if List.length xs = 2 then
                let e1 =
                    List.item 0 xs
                    |> function
                    | Symbol v -> v
                    | _ -> failInvalid (Some "def-var<1>")

                let e2 = List.item 1 xs
                let e2Compiled = compileSexpObject e2
                List.append e2Compiled [ OpVarDef e1 ]
            else
                failInvalid (Some "def-var<2>")
        | List (Symbol x :: xs) when x = "def-fun" ->
            if List.length xs = 3 then
                let funcName =
                    List.item 0 xs
                    |> function
                    | Symbol v -> v
                    | _ -> failInvalid (Some "def-fun<1>")

                let e2 =
                    List.item 1 xs
                    |> function
                    | List l -> l
                    | _ -> failInvalid (Some "def-fun<2>")

                let argNames =
                    List.map (function
                        | Symbol t -> t
                        | _ -> failInvalid (Some "def-fun<3>")) e2

                let funcBody =
                    xs.[2..]
                    |> (List.map compileSexpObject >> List.concat)

                let lvars = ref []

                let funcBody' =
                    let isOpSetVar =
                        function
                        | OpSetVar _ -> true
                        | _ -> false

                    let isOpVarDef =
                        function
                        | OpVarDef _ -> true
                        | _ -> false

                    List.map (fun op ->
                        match op with
                        | OpVarDef varName
                        | OpGetVar varName
                        | OpSetVar varName ->
                            let argNames = List.rev argNames

                            let found =
                                List.tryFindIndex (fun e -> e = varName) argNames

                            let argNamesLen = List.length argNames
                            (match found with
                             | None ->
                                 if isOpVarDef op || isOpSetVar op then
                                     let lvarIdx1 =
                                         if isOpVarDef op then
                                             (listAddRef lvars varName
                                              Some(List.length !lvars - 1))
                                         else
                                             (List.tryFindIndex (fun lvarName -> varName = lvarName) !lvars)

                                     match lvarIdx1 with
                                     | None -> op
                                     | Some lvarIdx ->
                                         let lvarIdx' = argNamesLen + lvarIdx
                                         OpSetLocal lvarIdx'
                                 else
                                     let foundLvarIdx =
                                         List.tryFindIndex (fun lvarName -> varName = lvarName) !lvars

                                     (match foundLvarIdx with
                                      | None -> op
                                      | Some lvarIdx ->
                                          let lvarIdx' = argNamesLen + lvarIdx
                                          OpGetLocal lvarIdx')
                             | Some foundIdx -> if isOpVarDef op then OpSetLocal foundIdx else OpGetLocal foundIdx)
                        | _ -> op) funcBody

                let allocaSize =
                    List.length argNames + List.length !lvars

                let funcBody = ref []
                listAddRef funcBody (OpAllocLvars(allocaSize))
                listAppendRef funcBody (List.mapi (fun i _ -> OpSetArgLocal i) argNames)
                listAppendRef funcBody funcBody'
                if allocaSize > 0 then listAddRef funcBody OpFreeVars
                listAddRef funcBody OpReturn

                let vmf =
                    { funcName = funcName
                      funcBody = !funcBody
                      argNames = argNames }

                [ OpFuncDef vmf ]
            else
                failInvalid (Some "def-fun<4>")
        | List (Symbol x :: xs) when x = "if" ->
            if List.length xs >= 2 then
                let condIns = List.item 0 xs |> compileSexpObject
                let tBlockIns = List.item 1 xs |> compileSexpObject
                let tBlockLen = OpCodesLen tBlockIns
                (* offset *)

                let offset =
                    tBlockLen
                    + if List.length xs = 3 then OpCodeWidth (OpJumpRel 0) else 0

                let ret =
                    List.append condIns (OpBranch offset :: tBlockIns)

                if List.length xs = 3 then (* exist fBlock *)
                    let fBlockIns = List.item 2 xs |> compileSexpObject
                    let fBlockLen = OpCodesLen fBlockIns
                    List.append ret (OpJumpRel(fBlockLen) :: fBlockIns)
                else
                    ret
            else
                failInvalid (Some "if")
        | List (Symbol x :: xs) when x = "begin" ->
            if List.length xs >= 1
            then (List.map compileSexpObject >> List.concat) xs
            else failInvalid (Some "begin")
        | List (Symbol x :: xs) when x = "while" ->
            if List.length xs = 2 then
                let condIns = List.item 0 xs |> compileSexpObject
                let exprIns = List.item 1 xs |> compileSexpObject
                (* offset *)

                let jmpOffset =
                    -(OpCodesLen exprIns
                      + 1 (* this jump *)
                      + OpCodesLen condIns
                      + 1 (* branch *) )

                let exprIns = exprIns @ [ OpJumpRel jmpOffset ]

                let condWithBranch =
                    List.append condIns [ OpBranch(OpCodesLen exprIns) ]

                List.append condWithBranch exprIns
            else
                failInvalid (Some "while")

        | List (Symbol x :: xs) when x = "for" ->
            (* (for init cond update expr) -> init [cond_with_branch: cond, block_ins: [expr, update]] *)
            if List.length xs = 4 then
                let initIns = List.item 0 xs |> compileSexpObject
                let condIns = List.item 1 xs |> compileSexpObject
                let updateIns = List.item 2 xs |> compileSexpObject
                let exprIns = List.item 3 xs |> compileSexpObject
                (* offset *)

                let jmpOffset =
                    -(OpCodesLen condIns
                      + 1 (* branch *)
                      + OpCodesLen exprIns
                      + OpCodesLen updateIns
                      + 1 (* jump to top *) )

                let blockIns =
                    exprIns @ updateIns @ [ OpJumpRel jmpOffset ]

                let condWithBranch =
                    List.append condIns [ OpBranch(OpCodesLen blockIns) ]

                initIns @ condWithBranch @ blockIns
            else
                failInvalid (Some "for")

        | List (Symbol x :: xs) when x = "set" ->
            if List.length xs = 2 then
                let varName =
                    List.item 0 xs
                    |> function
                    | Symbol t -> t
                    | _ -> failInvalid (Some "set")

                let exprIns = List.item 1 xs |> compileSexpObject
                List.append exprIns [ OpSetVar varName ]
            else
                failInvalid (Some x)
        | List (Symbol x :: xs) ->
            let t1 = List.collect compileSexpObject xs

            let t2 =
                match x with
                | "+" -> [ OpAdd ]
                | "-" -> [ OpSub ]
                | "*" -> [ OpMul ]
                | "/" -> [ OpDiv ]
                | "%" -> [ OpMod ]
                | "==" -> [ OpEq ]
                | "!=" -> [ OpNeq ]
                | "<" -> [ OpLt ]
                | "<=" -> [ OpLeq ]
                | ">" -> [ OpGt ]
                | ">=" -> [ OpGeq ]
                | _ -> [ OpCall(x, List.length xs) ]

            List.append t1 t2
        | _ ->
            printfn "given: %A" sexp
            failwith "invalid source code given ????"

    let compile = List.collect compileSexpObject

    let compileFromFile fileName =
        let content = System.IO.File.ReadAllText fileName
        let parsed = sexpParse content
        compile parsed
