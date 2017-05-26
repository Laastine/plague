module LogAgent

open System
open System.IO

type private Message =
  | Info    of string
  | Error   of string
  with
    static member toString logMessage =
      let d = DateTime.Now.ToLongTimeString()
      match logMessage with
      | Info    m -> m |> sprintf "%s INFO:%s" d
      | Error   m -> m |> sprintf "%s ERROR:%s" d

      override this.ToString() =
        Message.toString this

type private LogCommand =
  | Log of Message
  | Flush
  | Close of AsyncReplyChannel<unit>


type LogAgent(logFile:string) as this  =
  let writer = lazy(File.AppendText logFile)

  let agent = MailboxProcessor.Start (fun agent ->
    let rec loop(count) = async {
      let! cmd = agent.Receive()
      if writer.IsValueCreated then writer.Value.Flush()
      match cmd with
      | Log msg ->
          let count = count + 1
          let msg = Message.toString msg
          writer.Value.WriteLine msg
          return! loop(count)
      | Close reply ->
          this.doClose()
          reply.Reply(ignore())
          return ignore()
      | _ -> return! loop(count)
    }

      loop(0))

  interface IDisposable with
    member this.Dispose() = this.doClose()

  member private this.doClose() =
    let d = agent :> IDisposable
    d.Dispose()

    if writer.IsValueCreated then writer.Value.Dispose()

  member private this.log objToMessage obj =
      obj |> objToMessage |> LogCommand.Log |> agent.Post

  member this.error = this.log Error
  member this.info  = this.log Info

  member this.flush() = LogCommand.Flush |> agent.Post
  member this.close() = LogCommand.Close |> agent.PostAndReply
