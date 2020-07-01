# Quickscript: Scripting engine for DrRacket

Quickscript [[docs](http://pkg-build.racket-lang.org/doc/quickscript/index.html)] is a tool for DrRacket which allows to quickly and easily extend DrRacket features, without having to restart it.
The scripts are automatically accessible from a new menu in DrRacket.

The [slides from RacketCon 2018](https://github.com/Metaxal/quickscript/blob/master/docs/racketcon-2018-quickscript.pdf) provide more context on the why of Quickscript.


## 1. Installation

Quickscript (but not yet quickscript-extra) now comes bundled with DrRacket.

You may want to install [additional scripts](https://github.com/Metaxal/quickscript-extra), but Quickscript can also be used alone.

## 2. Usage

Have a look at the `Scripts` menu in DrRacket.

See the [docs](http://pkg-build.racket-lang.org/doc/quickscript/index.html) for more information.

## 3. History

Quickscript is the successor to [Script Plugin](https://github.com/Metaxal/script-plugin), with some differences:
- Each Quickscript script is a **single file** (instead of 2), which makes it easier to share and modify.
  - This is possible thanks to Racket's submodule system: the submodule containing the script's properties can be loaded without loading the script itself.
- There is still one main directory for the user's new scripts, but Quickscript can **look for scripts in various directories**.
- The **script library** can be managed with a **GUI**, making it easy to de/activate specific scripts, and add/remove script directories.
- Racket's **package** system can be used to easily **share sets of scripts**.
- **'Shadow' scripts** allow the user to install third-party scripts (e.g., from a package), change their properties for his/her needs, but still make sure these third-party scripts can be updated.
