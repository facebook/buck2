@echo off
:: batch equivalent of `ls "$(dirname "$0")"`
:: %~dp0 expands to drive letter + path of arg0
dir "%~dp0"
