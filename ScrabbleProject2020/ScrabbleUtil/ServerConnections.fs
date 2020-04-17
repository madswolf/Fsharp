namespace ScrabbleUtil

open System.IO

open MBrace.FsPickler

open FSharp.Quotations
open FSharp.Quotations.Evaluator

open System.Net
open System.Net.Sockets

module DebugPrint =
    type Message = 
    | DPrint of string * System.ConsoleColor
    | ForcePrint of string * System.ConsoleColor
    | Pause 
    | Resume


    let mutable debugFlag = false

    let printAgent () = MailboxProcessor.Start(fun inbox-> 

        // the message processing function
        let rec messageLoop paused msgs = async {

            let print s c =
                System.Console.ForegroundColor <- c
                printf "%s" s
                System.Console.ForegroundColor <- System.ConsoleColor.Black

        
            // read a message
            let! msg = inbox.Receive()
            match msg with
            | DPrint (s, c) when paused -> return! messageLoop paused ((s, c)::msgs)
            | DPrint (s, c)             -> print s c; return! messageLoop paused msgs
            | ForcePrint (s, c)         -> print s c; return! messageLoop paused msgs
            | Pause                     -> return! messageLoop true msgs
            | Resume                    -> 
                List.iter (fun (s, c) -> print s c) (msgs |> List.rev);
                return! messageLoop false []
        }

        // start the loop 
        messageLoop false []
    )

    let agent = printAgent()
    
    let debugPrintCol msg col = if debugFlag then agent.Post (DPrint (msg, col))
    let forcePrintCol msg col = agent.Post (ForcePrint (msg, col))
    
    let debugPrint msg = debugPrintCol msg System.ConsoleColor.Magenta
    let forcePrint msg = forcePrintCol msg System.ConsoleColor.Black

    let pause = agent.Post Pause
    let resume = agent.Post Resume

module ServerCommunication =    
        
    type ServerMessage =
        | SMPlay      of (coord * (uint32 * (char * int))) list
        | SMPass      
        | SMForfeit
        | SMChange    of uint32 list

    type ClientMessage =
        | CMPlayed             of uint32 * (coord * (uint32 * (char * int))) list * int
        | CMPlaySuccess        of (coord * (uint32 * (char * int))) list * int * (uint32 * uint32) list
        | CMPlayFailed         of uint32 * (coord * (uint32 * (char * int))) list
        | CMPassed             of uint32
        | CMForfeit            of uint32
        | CMChange             of uint32 * uint32
        | CMChangeSuccess      of (uint32 * uint32) list
        | CMTimeout            of uint32
        | CMGameOver           of (uint32 * int) list

    type GameplayError =
    | GPEOccupiedTile of (char * int) * coord * (char * int)
    | GPEWordNotOnRowOrColumn of coord list
    | GPEEmptyMove
    | GPEInvalidPieceInst of uint32 * (char * int)
    | GPEPieceDoesNotExist of uint32
    | GPEEmptyTile of coord
    | GPEPlayerDoesNotHavePiece of uint32 * uint32
    | GPEWordNotConnected
    | GPEWordOutsideBoard
    | GPEWordNotInDictionary of string
    | GPEFirstWordNotOverCenter of coord
    | GPEFirstWordTooShort
    | GPENotEnoughPieces of uint32 * uint32
    | GPEWordNotAdjacent

    type Response =
        | RCM  of ClientMessage
        | RGPE of GameplayError list 

    let formatter = FsPickler.CreateBinarySerializer()

    let send (cstream : Stream) msg =
        formatter.Serialize(cstream, <@ msg @>, leaveOpen = true)

    let recv (cstream : Stream) =
        formatter.Deserialize<Expr<Response>>(cstream, leaveOpen = true) |>
        QuotationEvaluator.Evaluate

    let trecv<'T> (cstream : Stream) =
        formatter.Deserialize<Expr<'T>>(cstream, leaveOpen = true) |>
        QuotationEvaluator.Evaluate