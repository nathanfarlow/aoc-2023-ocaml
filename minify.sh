set -euo pipefail

if [ $# -eq 0 ]; then
    echo "Usage: $0 <dir>"
    exit 1
fi

dir="$1"

dune build
true_part1=$(dune exec "$dir"/main.exe -- -part1 input.txt)
true_part2=$(dune exec "$dir"/main.exe -- -part2 input.txt)

reduce --char "$dir"/main.ml "\
if ! dune build; then
    exit 0
fi
part1=$(dune exec "$dir"/main.exe -- -part1 input.txt)
part2=$(dune exec "$dir"/main.exe -- -part2 input.txt)
if [[ \"\$part1\" == "$true_part1" && \"\$part2\" == "$true_part2" ]]; then
    exit 1
fi
exit 0
"
