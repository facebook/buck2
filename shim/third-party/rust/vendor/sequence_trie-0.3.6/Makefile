# Documentation upload Makefile.

USER=michael
HOST=sproul.io
DIR=/srv/http/gnuhacks/rust

doc-upload: target/doc/sequence_trie
	rsync -rtp --chmod=u=rwX,go+rX,go-w $< $(USER)@$(HOST):$(DIR)

target/doc/sequence_trie:
	cargo doc --no-deps

.PHONY: target/doc/sequence_trie
