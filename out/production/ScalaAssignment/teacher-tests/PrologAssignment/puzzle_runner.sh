cp /data/dat233/puzzles/$1 /app/puzzle_unsolved.txt
touch /app/puzzle_solved.txt
swipl -c PuzzleSolver.p*
cp /app/puzzle_solved.txt /data/dat233/answers/$2
