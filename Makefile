
.PHONY production:
production:
	lein do clean, cljsbuild once min
