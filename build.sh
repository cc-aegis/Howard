ghc -o ./target/howard src/main.hs -outputdir=dist
if [[ $1 = "run" ]] then
    ./target/howard
fi