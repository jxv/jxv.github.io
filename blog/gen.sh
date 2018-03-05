#! /bin/sh

stagen clean -e header.md -f footer.md -a index.md -c style.css -u http://jxv.io/blog -t "jxv blog"
stagen build -e header.md -f footer.md -a index.md -c style.css -v "../favicon.png" -u http://jxv.io/blog -t "jxv blog"
