#!/bin/bash

# Reset
Color_Off='\033[0m'       # Text Reset

# Regular Colors
Black='\033[0;30m'        # Black
Red='\033[0;31m'          # Red
Green='\033[0;32m'        # Green
Yellow='\033[0;33m'       # Yellow
Blue='\033[0;34m'         # Blue
Purple='\033[0;35m'       # Purple
Cyan='\033[0;36m'         # Cyan
White='\033[0;37m'        # White

# Loop through each file in examples/good directory
echo "-------------- GOOD EXAMPLES: --------------"
for file in examples/good/*; do
    echo -e "${Green}$file${Color_Off}"

    # get error of running src/interpreter on the file
    result=$(src/interpreter "$file" 2>&1)

    echo "$result"
done

# Loop through each file in examples/good directory
echo "-------------- BAD EXAMPLES: --------------"
for file in examples/bad/*; do
    echo -e "${Red}$file${Color_Off}"

    # get error of running src/interpreter on the file
    result=$(src/interpreter "$file")

    echo "$result"
done