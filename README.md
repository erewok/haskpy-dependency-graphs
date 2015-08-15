# haskpy-dependency-graphs
An attempt to build Python dependency graphs in Haskell

## WORK IN PROGRESS
This application was inspired by two things:

1. An interest in learning Haskell

2. A desire to map out dependencies in Python applications (and to find circular dependencies).

This project has just been started (and may not ever be completed), but in an effort to save current progress, it is being hosted here.

## Demo

A demo file has been included:

[Sample program output](http://htmlpreview.github.io/?https://github.com/pellagic-puffbomb/haskpy-dependency-graphs/blob/master/demo.html)

Note that normally nodes in the source will include full OS file paths.


## Problem Areas

* ~~Function `displayGraph` relies on route to index template. This won't work outside of source directory!~~

* ~~Only works on relative paths for the moment!~~

* Nothing from any `__init__.py` module is being examined!

* Graphs do not mark cycles, ~~or direction of edges.~~

* There is only minimal attempt at error handling at this point.

* No performance analysis has been done. This may not work for larger applications

* Graphs need some work to be more readable/useful.

* Will not work for Python modules with multiline imports.

* I have no idea how to write tests for Haskell code.

* Need to make invocation more flexible/intuitive (including various command-line options and scenarios).

* And more...

## How to Use

Installing dependency-graphs will create a `pydependencies` executable. In order to use it, you must have a `PYTHONPATH` declared (without a `PYTHONPATH` declared, the program will error out).

To invoke the program, after you have declared a PYTHONPATH that includes the module you care about, pass in the Python version and the file you would like to examine:

```
    pydependencies "python3.4" ./python_module > output.html
```

This will generate an `output.html` file, which can be opened in a web browser.

The `pydependencies` application will, by default, ignore all **third-party** and **python standard library** modules. Thus, if you try to examine a Python module inside a `site-packages` directory, the output will be empty.