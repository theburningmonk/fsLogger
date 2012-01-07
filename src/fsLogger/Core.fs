namespace fsLogger

open System
open System.IO

/// The different types of log messages
type LogMessage = 
    | Debug         of string
    | Info          of string
    | Warn          of string
    | Error         of string * Exception
    | Fatal         of string * Exception

/// Represents a logger
type ILogger =
    /// The all important method to log a message
    abstract member Log : LogMessage -> unit

// The base class for all loggers
type BaseLogger internal (name : string) =
    static let newLine = System.Environment.NewLine

    /// Virtual method for translating a 'LogMessage' to text
    abstract member GetLogText : LogMessage -> string
    default this.GetLogText logMsg = 
        match logMsg with
            | Debug msg         -> sprintf "%A [DEBUG] [%s] : %s%s" DateTime.Now name msg newLine
            | Info msg          -> sprintf "%A [INFO]  [%s] : %s%s" DateTime.Now name msg newLine
            | Warn msg          -> sprintf "%A [WARN]  [%s] : %s%s" DateTime.Now name msg newLine
            | Error (msg, ex)   -> sprintf "%A [ERROR] [%s] : %s%sEXCEPTION: %A%s" DateTime.Now name msg newLine ex newLine
            | Fatal (msg, ex)   -> sprintf "%A [FATAL] [%s] : %s%sEXCEPTION: %A%s" DateTime.Now name msg newLine ex newLine

/// An implementation of ILogger which logs to the console
type ConsoleLogger private (name) as this = 
    inherit BaseLogger(name)

    let agent = 
        Agent<LogMessage>.Start(fun inbox ->
            async {
                while true do
                    let! msg = inbox.Receive()
                    printfn "%s" <| this.GetLogText msg
            })

    interface ILogger with member this.Log logMsg = agent.Post logMsg

    static member Create (name) = ConsoleLogger(name) :> ILogger

/// An implementation of ILogger which logs to the file at the specified path
type FileLogger private (name, agent : Agent<string>) =
    inherit BaseLogger(name)

    static let mutable Agents = Map.empty<string, Agent<string>>

    // define a supervisor agent to handle errors emitted by the file logging agent
    // since we can't log to the file here..
    static let supervisor = Agent<Exception>.Start(fun inbox ->
        async {
            let logger = ConsoleLogger.Create(typeof<FileLogger>.Name)
            
            while true do
                let! err = inbox.Receive()
                logger.Log <| Error ("Unhandled exception in FileLogger", err)
        })

    static let createAgent filePath = 
        new Agent<string>(fun inbox ->
            async {            
                use fileStream = new FileStream(filePath, FileMode.Append)

                while true do
                    let! msg = inbox.Receive()
                    do! fileStream.AsyncWrite(msg.ToByteArray())
                    fileStream.Flush()
            }) 
        |> Agent.reportErrorsTo supervisor
        |> Agent.start

    static let getAgent filePath =
        if not <| Agents.ContainsKey filePath then Agents <- Agents.Add(filePath, createAgent filePath)
        Agents.[filePath]

    interface ILogger with member this.Log logMsg = agent.Post <| (this.GetLogText logMsg)
       
    static member Create (name, filePath) = 
        let agent = getAgent filePath
        FileLogger(name, agent) :> ILogger

/// An implementation of ILogger which proxies log messages to an array of children loggers
type MultiLogger private (loggers : ILogger list) =
    static member Create loggers = new MultiLogger(loggers) :> ILogger

    interface ILogger with member this.Log logMsg = loggers |> List.iter (fun l -> l.Log logMsg)

/// Manages the current configuration for loggers
type LoggerManager = 
    static member GetLogger (name) = 
        MultiLogger.Create([ ConsoleLogger.Create(name); FileLogger.Create(name, @"C:\temp\logging.txt") ])

    static member GetLogger (t : Type) = LoggerManager.GetLogger (t.Name)