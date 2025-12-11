default:
    @just --list

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ARGS}}

# Autoformat the project tree
fmt:
    treefmt

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid -c "cabal repl exe:dice-bot" --warnings -T :main

there:
    echo "sudo systemctl restart dice-bot.service" | ssh tub

here: stop
  nix run

stop:
  echo "sudo systemctl stop dice-bot.service" | ssh tub
