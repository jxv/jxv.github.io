#! /bin/sh

stagen clean -e header.md -f footer.md -a index.md -c style.css
stagen build -e header.md -f footer.md -a index.md -c style.css -v "../favicon.png"
