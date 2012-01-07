namespace fsLogger

/// Type abbreviation for a MailboxProcessor
type Agent<'T> = MailboxProcessor<'T>

[<RequireQualifiedAccess>]
module Agent =
    let reportErrorsTo (supervisor: Agent<exn>) (agent: Agent<_>) =
        agent.Error.Add(fun error -> supervisor.Post error); agent
  
    let start (agent: Agent<_>) = agent.Start(); agent

[<AutoOpen>]
module Extensions =
    type System.String with
        /// Converts string to byte[]
        member string.ToByteArray () = System.Text.Encoding.ASCII.GetBytes string
