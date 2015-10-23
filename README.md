

# Welcome to the Ranker library. #

__Authors:__ [`Lars-Ake Fredlund (lfredlund@fi.upm.es), Clara Benac Earle (cbenac@fi.upm.es)`](mailto:Lars-Ake Fredlund (lfredlund@fi.upm.es), Clara Benac Earle (cbenac@fi.upm.es)).

Ranker is a library that helps compare, using testing, how well different
implementations of the same specification behave, obtaining 
a "ranking" of the implementations (which are better, which are worse).
The library uses the Quviq QuickCheck tool for testing.


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

Should you wish to install the Ranker tool in the standard
Erlang library structure, the following commands can be used:<br />
```
$ erl -pa _build/default/lib/ranker/ebin/

> ranker_install:install().
```
</p>

<p>
To generate Markdown docs, execute the following command:<br/>
```
$ env ERL_LIBS=$PWD/_build/default/lib/edown rebar3 edoc
```



## Examples ##

The `examples` directory contains two examples, a set of Java based
implementations of a simple sorting algorithm in the directory java, and
a set of Erlang based implementations of a list reversal in the 
directory erlang.


### Java Example ###


A prerequisite for working with the Java example is having
the [JavaErlang](https://github.com/fredlund/JavaErlang.git) library installed. 
To build and test the Java example, after having compiled
the Ranker tool, follow the steps below:

```
examples/java$ make
examples/java$ erl -sname tst -pa ../../_build/default/lib/ranker/ebin/ -pa ebin

1> test:test().
...
2> ranker_render:render_classes("corr").
...
3> halt().
```

As a result of the ranking analysis there will be a set of files corr_classes_X.{bin,dot,interpretation,pdf} which describe the computed ranking (step-by-step).


### Erlang Example ###


To build and test the Erlang example, after having compiled
the Ranker tool, follow the steps below:

```
examples/erlang make
examples/erlang erl -pa ../../_build/default/lib/ranker/ebin/ -pa ebin

1> test:test().
...
2> ranker_render:render_classes("corr").
...
3> halt().
```

As a result of the ranking analysis there will be a set of files corr_classes_X.{bin,dot,interpretation,pdf} which describe the computed ranking (step-by-step).


## Extras ##


The directory extras contains a small interactive application,
based on the [gnuplot tool](http://www.gnuplot.info/),
to interact with complexity graphs computed by the
[Complexity tool](https://github.com/nick8325/complexity.git).
Consult the README file in the extras directory for further information.

