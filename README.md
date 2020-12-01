# Advent of Code 2020

Jeremy W. Sherman

This year's language is: [**Mercury**](https://mercurylang.org/), which is like
a statically-typed Prolog with a focus on fast execution.

This year's challenges can be found at: https://adventofcode.com/2020

## Building & Running

`mmc` drops a lot of stuff in the current directory, so you'll want to:

```
# fish syntax
cd build/
set -x DAY 1
mmc ../src/day$DAY.m && ./day$DAY < ../input/day$DAY.txt
```

There's probably some clever way to automate this with Make, but, meh.

## Versions

### Language

```
> mmc --version
Mercury Compiler, version 20.06, on x86_64-apple-darwin19.6.0
Copyright (C) 1993-2012 The University of Melbourne
Copyright (C) 2013-2020 The Mercury team
```

### OS

```
> sw_vers
ProductName:	Mac OS X
ProductVersion:	10.15.7
BuildVersion:	19H15
```
