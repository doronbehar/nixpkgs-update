#!/bin/sh

set -e

attr_requested="$1"
url="$2"
upstram_tags_prefix="$3"
upstram_tags_suffix="$4"
# upstram_tags_formater is not mandatory
if [ $# -lt 2 ] || [ $# -gt 4 ]; then
	echo usage: "$0" "<attribute>" "<rss url>" ["<upstram tags' common prefix>"] ["<upstram tags' common suffix>"]
	echo
	echo note the prefix and the suffix are interpreted by \`sed\` so you can use it\'s supported patterns
	exit 2
fi

verbose="$NIXPKGS_UPDATE_VERBOSE"

# This is probably done somehow inside nixpkgs-update but I don't know haskell
# enough in order to extract that from there and use it
while read attr version; do
	if [[ "$attr" == "$attr_requested" ]]; then
		current_version="$version"
		found_attr=1
		break
	fi
done < package-version.tsv
if !(( $found_attr )); then
	echo "Couldn\'t find an attribute named $attr_requested in nixpkgs!" >&2
	exit 1
fi
(( "$verbose" )) && echo $attr_requested: current version in nixpkgs: "$current_version"

# We iterate over the versions currently available upstream just so we'll remember the last one
# TODO: Check for versions we shouldn't update, according to:
# https://github.com/repology/repology-rules

IFS=$'\n'
# The sed filter removes 'Title: ' lines from rsstail's output and returns the
# last component of the url of the line starting with Link: ...
for version in $(rsstail -1 -u "$url" -H -l -r | sed -e '/^Title: /d' -e 's:.*/::'); do
	if [ -n "${upstram_tags_prefix}" ]; then
		version="$(echo $version | sed -e s,^${upstram_tags_prefix},,)"
	fi
	if [ -n "${upstram_tags_suffix}" ]; then
		version="$(echo $version | sed -e s,${upstram_tags_suffix}$,,)"
	fi
done
(( "$verbose" )) && echo "$attr_requested: upstram version (formatted) is: $version"

# TODO: emulate/use nix-env --compare-versions like function instead of !=
if [[ "$version" != "$current_version" ]]; then
	echo "$attr_requested" "$version"
fi
