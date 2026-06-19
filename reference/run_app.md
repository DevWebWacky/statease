# Launch the statease Shiny App

Launches an interactive Shiny application for running statistical
analyses without writing any code.

## Usage

``` r
run_app(...)
```

## Arguments

- ...:

  Additional arguments passed to shiny::runApp()

## Value

Launches the Shiny app in your browser

## Examples

``` r
if(interactive()){
  run_app()
}
```
