#! /bin/sh

# stagen clean -e header.md -f footer.md -a index.md -c style.css -c ../dark-mode.css -u https://joevargas.me/blog -t "Joe Vargas"
stagen build -e header.html -i header.md -f footer.md -a index.md -c style.css -c ../dark-mode.css -s ../dark-mode.js -v "../favicon.png" -u https://joevargas.me/blog -t "Joe Vargas"
