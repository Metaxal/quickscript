# Quickscript: A script system for DrRacket

Quickscript is a tool for DrRacket which allows to quickly and easily extend DrRacket features, without having to restart it.
The scripts are automatically accessible from a new menu in DrRacket.

[[Slides from RacketCon 2018](https://drive.google.com/open?id=1ZDtEZ5XIWemWXC5L4Qu-FhuDieRPHwvU)]

*Note:* Quickscript (but not yet quickscript-extra) now comes bundled with DrRacket.

## 1. Installation

* From DrRacket:
In DrRacket, in `File|Package` `manager|Source`, type
`https://github.com/Metaxal/quickscript.git`.

* Or from the command line:
```
raco pkg install https://github.com/Metaxal/quickscript.git
```

You will need to restart DrRacket. A new `Scripts` menu should appear.

You may want to install [additional scripts](https://github.com/Metaxal/quickscript-extra), but Quickscript can also be used alone.

## 2. Usage

Have a look at the new menu.

See the [docs](http://pkg-build.racket-lang.org/doc/quickscript/index.html) for more information.

## 3. History

Quickscript is the successor to [Script Plugin](https://github.com/Metaxal/script-plugin), with some differences:
- Each Quickscript script is a **single file** (instead of 2), which makes it easier to share and modify.
  - This is possible thanks to Racket's submodule system: the submodule containing the script's properties can be loaded without loading the script itself.
- There is still one main directory for the user's new scripts, but Quickscript can **look for scripts in various directories**.
- The **script library** can be managed with a **GUI**, making it easy to de/activate specific scripts, and add/remove script directories.
- Racket's **package** system can be used to easily **share sets of scripts**.
- **'Shadow' scripts** allow the user to install third-party scripts (e.g., from a package), change their properties for his/her needs, but still make sure these third-party scripts can be updated.
