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

```
$ ploton '#1 u 1:2;#1 u 1:3;' --multi "1,2" --title "n^2;n log(n)" sample-multi-2d.dat
```

```
$ ploton '#1 u 1:2:3;#1 u 1:2:4' -3 --set "size square" --multi "1,2" --color "jet" sample-multi-3d.dat
```
