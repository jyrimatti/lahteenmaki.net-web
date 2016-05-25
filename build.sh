#!/bin/sh
set -eu

./index.html.hs > index.html.tmp && mv index.html.tmp index.html
./style.css.hs > style.css.tmp && mv style.css.tmp style.css
