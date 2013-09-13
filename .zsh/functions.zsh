# Back up a file
bu () {
	cp -r $1 ${1}_backup_`date +%Y-%m-%d-%H:%M` ;
}
