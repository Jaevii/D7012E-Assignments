# D7012E Assignments
Haskell and Prolog assignments for the course [D7012E Declarative Languages](https://www.ltu.se/en/education/course/d70/d7012e-declarative-languages) for fourth year students in Computer Science at Luleå tekniska universitet.

## Haskell
### Requires GHCup

Install on Windows in PowerShell using:
```bash
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
``` 

Or on MacOS, Linux using:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
``` 

Run programs with:
```bash
runhaskell file.hs
``` 

Or compile and run programs using the GHCi terminal with:
```bash
ghci file.hs
``` 

### Some "good-to-know" GHCi commands:

`:quit` or `Ctrl+D`: Exits the GHCi-terminal

`Ctrl+L`: Clear the GHCi-terminal

## Prolog
### Requires SWI-Prolog
Download SWI-Prolog [here](https://www.swi-prolog.org/download/devel)

Enter the swipl-terminal to run programs:
```bash
swipl file.pl
``` 

Or use the shell script:
```bash
./run.sh
``` 

### Some "good-to-know" swipl commands:

`foo.`: Runs the function named "foo"

`halt.`: Exits the swipl-terminal