tgz:
	git archive --format=tar --prefix=av-prog-jlouis-brinch-kjaer-final/ master \
	  --worktree-attributes -v \
	  | gzip >av-prog-jlouis-brinch-kjaer-final.tar.gz
