# Quickscript: Scripting engine for DrRacket

Quickscript [[docs](https://www.cs.utah.edu/plt/snapshots/current/doc/quickscript/index.html)] is a tool for DrRacket which allows to quickly and easily extend DrRacket features, without having to restart it.
The scripts are automatically accessible from a new menu in DrRacket.

The [slides from RacketCon 2018](https://github.com/Metaxal/quickscript/blob/master/docs/racketcon-2018-quickscript.pdf) provide more context on the why of Quickscript.


## 1. Installation

Quickscript comes bundled with DrRacket.

You may want to install [additional scripts](https://github.com/Metaxal/quickscript-extra) (also [here](https://github.com/racket/racket/wiki/Quickscript-Scripts-for-DrRacket)), but Quickscript can also be used alone.

## 2. Usage

Have a look at the `Scripts` menu in DrRacket.

See the [docs](https://www.cs.utah.edu/plt/snapshots/current/doc/quickscript/index.html) for more information.

## 3. Developing Quickscript

If you want to modify how Quickscript works, here's how to replace the bundled collection with the development one:

1. Fork [quickscript on github](https://github.com/Metaxal/quickscript)
2. In some local folder on your computer, clone from the forked repo:
```shell
git clone https://github.com/<your-github-username>/quickscript.git
```
3. Without changing directory, 
```shell
raco pkg update --link quickscript
```
This may require `sudo` if it was used when installing racket.

Now the quickscript collection refers to the cloned repository.


## 4. History

Quickscript is the successor to [Script Plugin](https://github.com/Metaxal/script-plugin), with some differences:
- Each Quickscript script is a **single file** (instead of 2), which makes it easier to share and modify.
  - This is possible thanks to Racket's submodule system: the submodule containing the script's properties can be loaded without loading the script itself.
- There is still one main directory for the user's new scripts, but Quickscript can **look for scripts in various directories**.
- The **script library** can be managed with a **GUI**, making it easy to de/activate specific scripts, and add/remove script directories.
- Racket's **package** system can be used to easily **share sets of scripts**.
- **'Shadow' scripts** allow the user to install third-party scripts (e.g., from a package), change their properties for his/her needs, but still make sure these third-party scripts can be updated.
