ARCHIVE=psci.el

pr:
	hub pull-request -b ardumont:master

install-clean:
	~/bin/emacs/emacs-install-clean.sh ./$(ARCHIVE)
