# Purpose

Teams has Attendance Reports for meetings. The `data` folder has some examples. For a recurring meeting this program generates an aggregate overview for all meeting reports passed as an argument.

# Missing Features & Known Bugs

Missing Features:
- Help & parsing command line parameters.
- Skip failed file instead of fail completely.
- Error messages go to stdout, not stderr.
- Map e-mails to friendly names and company. Sort by company and then name. Add order for companies.
    
Bugs:
- Dates are not sorted correctly. Last column seems wrong.

# Prepare files
Files from Teams are in  UTF-16, need to convert them to UTF-8.
```bash
mkdir utf8
for F in *.csv; do iconv -f UTF-16 -t UTF-8 "$F" >"utf8/$F" ; done
cd utf8
teams-attendance-exe *.csv > iDL.csv
```


# Build and Install
```bash
stack path --local-bin
stack install
```


# Development
## Set Up

See https://medium.com/@dogwith1eye/setting-up-haskell-in-vs-code-with-stack-and-the-ide-engine-81d49eda3ecf

## Test Run
Using test files in `data` folder:
```bash
stack run $(find data/*.csv)
```

## Stack & Cabal Libraries
Open https://www.stackage.org/lts-20.12 and search for the type, e.g. `optparse`. You may find a package `optparse-applicative`, add that to `package.yaml` packages list.

## Upgrade ghc, cabal etc.

Update the resolver field of the `stack.yaml` to use the latest LTS Haskell, then stack build.
Best to comment out all libraries in `package.yaml` first.

Note also stack.yaml of the “global project” (e.g. ~/.stack/global-project/stack.yaml).