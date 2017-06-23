# ploton
A useful cli tool to draw figures

## Requirement
This tool needs

- gnuplot

## Install

```
$ stack update
$ stack install ploton
```

## Usage

```
$ ploton --xl "x" --yl "x^2" '#1 u 1:2 with linespoints' "sample.dat"
plot_result.pdf
```

See [sample.dat](./sample.dat) and [plot\_result.pdf](./plot_result.pdf).
