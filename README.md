

# Welcome to the Ranker library. #

__Authors:__ [`Lars-Ake Fredlund (lfredlund@fi.upm.es), Clara Benac Earle (cbenac@fi.upm.es)`](mailto:Lars-Ake Fredlund (lfredlund@fi.upm.es), Clara Benac Earle (cbenac@fi.upm.es)).

Ranker is a library that helps compare, using testing, how well different
implementations of the same specification behave, obtaining 
a "ranking" of the implementations (which are better, which are worse).
The library uses Quviq QuickCheck test
tool for testing.


## Build, Test, and Generate Markdown Docs ##

Ranker requires [rebar3](http://www.rebar3.org) for
building and testing.  See [here](http://www.rebar3.org/v3.0/docs/getting-started) for
getting started with rebar3.

To compile, execute the following command:<br />

```
$ rebar3 compile
```


After compilation Erlang beam files will be left in the
directory _build/default/lib/ranker/ebin/.

To generate Markdown docs, execute the following command:<br />

```
$ env ERL_LIBS=$PWD/_build/default/lib/edown rebar3 edoc
```



## Examples ##

The `examples` directory contains a number of examples.

