# D7012E Assignments
Haskell and Prolog assignments for the course [D7012E Declarative Languages](https://www.ltu.se/en/education/course/d70/d7012e-declarative-languages) for fourth year students in Computer Science at Luleå tekniska universitet.

## Haskell
Requires GHCup

Install on Windows in PowerShell using:
```bash
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
``` 

Or on MacOS, Linux using:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
``` 

Compile and run programs with:
```bash
runhaskell program_name.hs
``` 

## Prolog
Coming soon