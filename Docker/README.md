# Purpl-State-Container

## Purpose
For portability we have containerized this project.
This guide will show you on how to get started with it.


## What is Included
In this directory you will find the **Dockerfile**.
The **Dockerfile** fetches haskell:9.6-slim.


## Get Started (Environment)
Note that this should only be run once.
To get started, start at the root of the repo.
From there run the following command.

``` bash
foo@bar:./purpl-state$ cd Docker
foo@bar:./purpl-state$ docker build --no-cache -t purpl -f Dockerfile ..
foo@bar:./purpl-state$ cd ..
```

## Running Docker
Once you have configured the environment as described above, you simply have to run the following command every time you would like to run Purpl. The name **purpl** is given for convenience and you may rename it as wish.
``` bash
foo@bar:./purpl-state$ docker run -it --rm -v $(pwd):/root/workspace purpl
```

## Running Purpl

``` bash
foo@bar:./purpl-state$ docker run -it --rm -v $(pwd):/root/workspace purpl
```

Inside the container the following

``` bash
root@abcdefg:~/workspace# cabal build --write-ghc-environment-files=never
```

To type check a Purpl file run the following

``` bash
root@abcdefg:~/workspace# cabal run Purpl -- Example/1_method_with_this.java 
```