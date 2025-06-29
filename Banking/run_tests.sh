#!/bin/bash

echo "======================================"
echo "ERLANG BANKING SIMULATION TEST SUITE"
echo "======================================"

# Compile the Erlang files
echo "Compiling Erlang files..."
erlc *.erl

if [ $? -ne 0 ]; then
    echo "ERROR: Compilation failed!"
    exit 1
fi

echo "Compilation successful!"
echo ""

# Test 1: Basic functionality
echo "TEST 1: Basic Functionality"
echo "Customers: alice(100), bob(150), charlie(200) = 450 total"
echo "Banks: citybank(200), westbank(300), eastbank(250) = 750 total"
echo "Expected: All customers should get their full amounts"
echo "Running..."
erl -noshell -run money start test_basic_c.txt test_basic_b.txt -s init stop
echo "Press Enter to continue..."
read

# Test 2: High demand vs limited funds
echo "TEST 2: High Demand vs Limited Funds"
echo "Customers: 5 customers wanting 16,000 total"
echo "Banks: 3 banks with only 2,800 total"
echo "Expected: Not all customers will get full amounts"
echo "Running..."
erl -noshell -run money start test_highdemand_c.txt test_highdemand_b.txt -s init stop
echo "Press Enter to continue..."
read

# Test 3: Large scale test
echo "TEST 3: Large Scale Test"
echo "Customers: 15 customers wanting ~3,840 total"
echo "Banks: 8 banks with 6,300 total"
echo "Expected: All customers should get their full amounts"
echo "Running..."
erl -noshell -run money start test_largescale_c.txt test_largescale_b.txt -s init stop
echo "Press Enter to continue..."
read

# Test 4: Single customer huge loan
echo "TEST 4: Single Customer Huge Loan"
echo "Customers: billionaire(25000), student(50), worker(200)"
echo "Banks: 4 banks with 12,150 total"
echo "Expected: Billionaire won't get full amount, others should"
echo "Running..."
erl -noshell -run money start test_bigloan_c.txt test_bigloan_b.txt -s init stop
echo "Press Enter to continue..."
read

# Test 5: Insufficient funds edge case
echo "TEST 5: Insufficient Funds Edge Case"
echo "Customers: 4 customers wanting 1,150 total"
echo "Banks: 3 banks with only 230 total"
echo "Expected: Most customers won't get their full amounts"
echo "Running..."
erl -noshell -run money start test_insufficient_c.txt test_insufficient_b.txt -s init stop
echo "Press Enter to continue..."
read

echo "======================================"
echo "ALL TESTS COMPLETED!"
echo "======================================" 