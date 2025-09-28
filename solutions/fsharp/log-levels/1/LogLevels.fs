module LogLevels

let message (logLine: string): string = 
    let index = logLine.IndexOf(':')
    logLine[index+1..].Trim()

let logLevel(logLine: string): string =
    let index = logLine.IndexOf(':')
    logLine[1..index-2].ToLower()

let reformat(logLine: string): string = 
    sprintf "%s (%s)" (message logLine) (logLevel logLine)
