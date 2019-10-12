#!/bin/sh

set -e

# Update the git repo
if ! git -C ~/.cache/nixpkgs pull | grep -q 'Already up to date'; then
	# if it's not "up to date", regenerate / update package-version.tsv so
	# ./print-if-new.sh will be able to read from it
	nix-env --file ~/.cache/nixpkgs --query --available --json | \
		jq --raw-output 'to_entries[] | [.key, .value.version] | @tsv' > package-version.tsv
else
	echo Note: Using not updating package-version.tsv since local checkout ~/.cache/nixpkgs wasn\'t updated >&2
fi

IFS=$'\t'
while read attr_requested url tags_prefix tags_suffix; do
	# Skip comments
	if [[ "$attr_requested" =~ '#' ]]; then
		continue
	fi
	./print-if-new.sh "$attr_requested" "$url" "$tags_prefix" "$tags_suffix" &
done < attr-rss.tsv
wait
