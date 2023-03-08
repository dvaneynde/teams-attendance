# Purpose

Teams has Attendance Reports for meetings. The `data` folder has some examples. For a recurring meeting this program generates an aggregate overview for all meeting reports passed as an argument.

# Missing Features & Known Bugs

Missing Features:
- Skip failed file instead of fail completely.
- Error messages go to stdout, not stderr.
    
Bugs:
- Dates are not sorted correctly. Last column seems wrong.

# Usage
Teams, open the meeting, and export the attendance report. This is a .csv file, in (stupid) UTF-16 format. The `data` folder has some examples.

To generate the summary report, execute:
```bash
teams-attendance-exe --utf16 *.csv
```
Without the `--utf16` option UTF-8 is assumed. To convert UTF-16 to UTF-8 you can also use `iconv`.

To replace the e-mails with a more readable name and company, use a UserMap file. Below is an example `usermap.txt`, 
that maps emails found in the .csv report to Jan or Johnny an their company. If the .csv report contains an e-mail
not in the usermap, output is the same as without UserMap.

```
UserMap {name = "Jan", company = "UN", eMails = ["jan.janssens.ext@un.int","sdc@smallco.co.uk"]}
UserMap {name = "Johnny", company = "Scotch", eMails = ["Johnny.Walker@scotch.aero"]}
```

To generate using the UserMap:
```bash 
teams-attendance-exe --utf16 -u usermap.txt *.csv
```

# Development

## Set Up

See https://medium.com/@dogwith1eye/setting-up-haskell-in-vs-code-with-stack-and-the-ide-engine-81d49eda3ecf

## Test Run
Using test files in `data` folder:
```bash
stack run -- -h
stack run -- --utf16 $(find data/*.csv)
```

## Install
```bash
stack path --local-bin
stack install
```

## Stack & Cabal Libraries
Open https://www.stackage.org/lts-20.12 and search for the type, e.g. `optparse`. You may find a package `optparse-applicative`, add that to `package.yaml` packages list.

## Upgrade ghc, cabal etc.

Update the resolver field of the `stack.yaml` to use the latest LTS Haskell, then stack build.
Best to comment out all libraries in `package.yaml` first.

Note also stack.yaml of the “global project” (e.g. ~/.stack/global-project/stack.yaml).