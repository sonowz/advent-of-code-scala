# Run code in a certain day.
# example: ./run.sh 2022 01

if [ $# -ne 2 ]; then
    echo "ERROR: Please provide year and day (example: $0 2022 01)"
    exit -1
fi

YEAR=$1
DAY=$2

if [[ ! -f "src/main/scala/com/sonowz/aoc/y$YEAR/Day$DAY.scala" ]]; then
    echo "ERROR: y$YEAR/Day$DAY.scala file does not exist."
    exit -1
fi

echo "Running y$YEAR.Day$DAY..."
echo ""

sbt "runMain com.sonowz.aoc.y$YEAR.Day$DAY"
