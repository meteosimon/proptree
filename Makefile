README.md: README.Rmd
	Rscript -e "rmarkdown::render('$<', output_format = 'md_document')"

site: README.md
	Rscript -e 'pkgdown::build_site()'

doc:
	Rscript -e 'devtools::document()'

install:
	Rscript -e 'devtools::install()'

check:
	Rscript -e 'devtools::check()'


