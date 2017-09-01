find src -name '*.hs' | xargs graphmod -q | dot -Tpng -o out.png
