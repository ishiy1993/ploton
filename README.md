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

```
$ ploton "#1" --style "pm3d" -3 --output "sample-3d.pdf" "sample-3d.dat"
sample-3d.pdf
```
