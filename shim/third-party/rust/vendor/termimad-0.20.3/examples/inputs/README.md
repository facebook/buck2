This example demonstrates
 - a responsive layout
 - a scrollable markdown text
 - a single line input field
 - a password input
 - a textarea
 - handling key and mouse events
 - managing the focus of widgets
 - selecting with shift-arrows
 - cut/copy/paste with <kbd>ctrl</kbd>-<kbd>X</kbd>, <kbd>ctrl</kbd>-<kbd>C</kbd>, <kbd>ctrl</kbd>-<kbd>V</kbd>
 - managing a terminal properly configured in "alternate" mode
 - logging events in a file (useful for event handling debugging)

Run this example with

    cargo run --example inputs

If you want to have a log file generated, run

    TERMIMAD_LOG=debug cargo run --example inputs

Quit with <kbd>ctrl</kbd>-<kbd>Q</kbd>

If you're on linux and there's a compilation error you may have to install xorg-dev and libxcb-composite0-dev, which can be done on apt based distributions with

    sudo apt install xorg-dev libxcb-composite0-dev
