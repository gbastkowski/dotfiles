#!/bin/bash
doom upgrade && doom sync --rebuild

systemctl --user restart emacs     && \
	echo emacs restarted       || \
	echo emacs restart failed

