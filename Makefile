.PHONY: docs

docs:
	cd examples; elm make Example.elm --output ../obj/example.js
	cat obj/example.js third/highlight.pack.js > docs/example.js
	cd examples; static-html-from-elm -f IndexPage.elm -o ../docs/index.html
	sass --sourcemap=none examples/example.scss docs/example.css
	cp examples/highlight.min.css docs/
	cp src/i18n/en.json docs/


