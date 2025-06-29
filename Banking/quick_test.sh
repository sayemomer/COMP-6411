#!/bin/bash

if [ $# -ne 2 ]; then
    echo "Usage: $0 <customer_file> <bank_file>"
    echo "Example: $0 c1.txt b1.txt"
    exit 1
fi

CUSTOMER_FILE=$1
BANK_FILE=$2

echo "Compiling Erlang files..."
erlc *.erl

if [ $? -ne 0 ]; then
    echo "ERROR: Compilation failed!"
    exit 1
fi

echo "Running test with $CUSTOMER_FILE and $BANK_FILE"
echo "======================================"
erl -noshell -run money start $CUSTOMER_FILE $BANK_FILE -s init stop 