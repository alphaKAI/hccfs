namespace HccFS

module OpCode =
    open Ksexp

    type VMValue =
        | VValue of SexpObject
        | VFunc of VMFunction

    and VMFunction =
        { funcName: string
          funcBody: OpCode list
          argNames: string list }

    and OpCode =
        | OpPop
        | OpPush of VMValue
        | OpAllocLvars of int
        | OpFreeVars
        | OpGetLocal of int
        | OpSetLocal of int
        | OpSetArgLocal of int
        | OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpMod
        | OpEq
        | OpNeq
        | OpLt
        | OpLeq
        | OpGt
        | OpGeq
        | OpPrint
        | OpPrintln
        | OpJumpRel of int
        | OpFuncDef of VMFunction
        | OpCall of string * int
        | OpReturn
        | OpVarDef of string
        | OpGetVar of string
        | OpSetVar of string
        | OpBranch of int
        | OpMakeList of int
        | OpSetArgFrom of string * int
        | OpDumpEnv
