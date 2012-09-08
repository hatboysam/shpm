# Simple Haskell Project Manager (SHPM)

### What is it?
It's like todo.txt, but I made it in Haskell and it's much simpler

### Why should I use it over todo.txt?
You probably shouldn't if you're a power user, but if you're a simple-case user like me you might like it better

### How do I install it?
I've uploaded both the Haskell source as well as an executable.  The easiest way is just to add the "shpm" file to your PATH.  If you want to compile the Haskell yourself, you'll first need to install the ANSI Terminal module by running 'sudo cabal install ansi-terminal'.  The ANSI Terminal module provides color support, for more information on the module visit the <a href="http://batterseapower.github.com/ansi-terminal/">homepage</a>.

### How do I use it?
shpm list -> list all tasks by project (in color)

shpm list -nc -> list all tasks by project (no color)

shpm add "Some Task" -> add "Some Task" to project "Other"

shpm add "Some Project" "Some Task" -> add "Some Task" to the project "Some Project", creating the project if necessary

shpm remove -> list all tasks and choose one to remove

shpm remove n -> remove task #n from the list

### Is that all?
Yep, for now.  Soon I'll be adding task grouping by project and other cool features.  Stay tuned.
