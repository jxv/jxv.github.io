#! /bin/sh

stagen clean -e header.md -f footer.md -a index.md -c style.css -u https://joevargas.me/blog -t "Joe Vargas"
stagen build -e header.md -f footer.md -a index.md -c style.css -v "../favicon.png" -u https://joevargas.me/blog -t "Joe Vargas"
