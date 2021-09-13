#!/bin/bash

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Configuration:
# Change the paths!
# Insert the path to validationdata here
teacher_test_path="/Users/kamiloracz/Documents/Skole/ScalaAssignment/teacher-tests/ValidationData"

# Challenge mode (optional)
#teacher_test_path="/home/per/git/ikt212-2021/teacher-tests/ValidationData/challenges/"

########### Config end ######################

# Compile your puzzle solver
scalac PuzzleSolver.scala

# Create answer directory if not exists
mkdir -p $teacher_test_path/answers

# Loop over all puzzles
for test_file in $teacher_test_path/puzzles/*.txt
do
  # Replace puzzle => solution and puzzle => anwer
  solution_file=${test_file/puzzles/solutions}
  answer_file=${test_file/puzzles/answers}

  # Remove newline and accidental whitespaces.
  #truncate -s -1 $solution_file
  #truncate -s -1 $answer_file

  # Test you program on a puzzle
  #echo "Testing program on puzzle: $test_file..."
  scala PuzzleSolver $test_file $answer_file > /dev/null

  # Validate if the puzzle is solved
  diffline=$(diff $answer_file $solution_file)

  # If diff return empty, yes then you have solved the puzzle.
  if [ -z "$diffline" ]
  then
       echo -e "Status: ${GREEN}PASSED${NC} on $test_file"
  else
       echo -e "Status: ${RED}FAILED${NC} on $test_file"
       echo $diffline
  fi
  unset diffline

done
