# Your fnmate

Type a call to a function and have a definition for that function appear with a keystroke. Lay the planks with your `fnmate` but leave the hammering til after smoko.

![fnmate](inst/media/fnmate.gif)

# Installation

```r
remotes::install_github("milesmcbain/fnmate")
```

# Usage

## RStudio

There are two addins that do pretty much what their names suggest:

  * `Create function definition file`
  * `Create function definition below`
    - as in appended to the end of the current file. Roxygen is not added in this case.
 
You can bind these to key combinations of your choosing, see [here](https://rstudio.github.io/rstudioaddins/#keyboard-shorcuts). 
  
The function to be generated is determined by the cursor position.  The function call at the most nested scope that encloses the cursor is the one that `fnmate` will generate a definition for.

## Emacs (ESS)

There is some Emacs lisp you can use to create bindings to `fnmate` contained in the vignette *Using fnmate with ESS*. If you have better ideas about how to distribute ESS 'addins' please let me know!

## Options

There are some options that affect how `fnmate` works:

  * `fnmate_window` determines how many lines above and below the current cursor position `fnmate` will look for a function call that encloses the cursor. Defaults to 20 which probably covers three standard deviations of coding styles.
  * `fnmate_folder` is the name of the folder in the current working directory to place created definition files. Defaults to "R".
  * `fnmate_placeholder` is the placeholder value that gets put in the function body. Defaults to `NULL`, can be set to any text.
  
# Why does this exist?

A lot of the time when I attack a problem I find it helps to cruise over the gnarly bits requiring fiddly code by just claiming a function exits that will magically resolve that fiddly bit for me. After I have a high level solution described, I go bCK and fill in the blanks. This tool helps me clearly mark out the blanks without breaking my flow on the higher level algorithm.

Recently when developing R workflow plans with [`drake`](https://github.com/ropensci/drake) I've found myself wanting a tool like this so that my sketch of the workflow plan can be built, even though some of the targets a just placeholders.

Also realising this idea in a robust way turned out to be way more challenging than I anticipated and necessitated coopting the R parser and its `parseData` output. So it became a learning exercise.
