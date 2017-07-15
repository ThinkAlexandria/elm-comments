.PHONY: docs

docs:
	elm-make examples/Example.elm --output obj/example.js
	cat obj/example.js third/highlight.pack.js > docs/example.js
	elm-static-html -f examples/IndexPage.elm -o docs/index.html
	sass --sourcemap=none examples/example.scss docs/example.css
	cp examples/highlight.min.css docs/
	cp src/i18n/en.json docs/


