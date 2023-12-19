# infinite-xo

FIXME: Write a one-line description of your library/project.

## Overview

FIXME: Write a paragraph about the library/project and highlight its goals.

## Setup

To get an interactive development environment, run:

```bash
lein figwheel
```
Open your browser at localhost:3449. This will auto-compile and send all changes to the browser without the need to reload. After the compilation process is complete, you will get a Browser Connected REPL. An easy way to try it is:

```clojure
(js/alert "Am I connected?")
```

Certainly! Here's the README with everything inside a code block:

To clean all compiled files:
```bash
lein clean
```

To create a production build, run:
```bash
lein do clean, cljsbuild once min
```

