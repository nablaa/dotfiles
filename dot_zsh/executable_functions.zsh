# Back up a file
bu () {
	cp -r $1 ${1}_backup_`date +%Y-%m-%d-%H:%M` ;
}

parse() {
	for f in "$@" ; do
		cat "$f" | cut -d " " -f 2- > "${f}_parsed"
	done
}

gpush() {
	git push --set-upstream origin "$(git rev-parse --abbrev-ref HEAD)"
}
