# Purpose

Teams has Attendance Reports for meetings. The `data` folder has some examples. For a recurring meeting this program generates an aggregate overview for all meeting reports passed as an argument.

# Known bugs

None so far.

# Prepare files
Files from Teams are in  UTF-16, need to convert them to UTF-8.
```bash
$ mkdir utf8
$ for F in *.csv; do iconv -f UTF-16 -t UTF-8 "$F" >"utf8/$F" ; done
```
# Development
## Set Up

See https://medium.com/@dogwith1eye/setting-up-haskell-in-vs-code-with-stack-and-the-ide-engine-81d49eda3ecf

## Run
Using test files in `data` folder:
```bash
ls data | sed "s/^/\"data/;s/$/\"/" | xargs stack run
```