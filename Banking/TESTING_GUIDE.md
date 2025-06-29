# Erlang Banking Simulation - Testing Guide

## Prerequisites
- Ensure Erlang is installed: `brew install erlang` (on macOS)
- All `.erl` files are in the same directory
- Test data files are present

## Quick Start Testing

### Method 1: Run All Tests
```bash
./run_tests.sh
```

### Method 2: Run Individual Tests  
```bash
./quick_test.sh <customer_file> <bank_file>
# Example:
./quick_test.sh c1.txt b1.txt
```

### Method 3: Manual Testing
```bash
erlc *.erl
erl -noshell -run money start <customer_file> <bank_file> -s init stop
```

## Test Scenarios Explained

### Test 1: Basic Functionality (test_basic_*)
- **Customers**: alice(100), bob(150), charlie(200) = 450 total
- **Banks**: citybank(200), westbank(300), eastbank(250) = 750 total  
- **Expected**: All customers get full amounts, banks have remaining funds
- **Validates**: Basic message passing, loan approval logic

### Test 2: High Demand (test_highdemand_*)
- **Customers**: 5 customers wanting 16,000 total
- **Banks**: 3 banks with only 2,800 total
- **Expected**: Customers compete for limited funds, not all get full amounts
- **Validates**: Resource scarcity handling, bank fund depletion

### Test 3: Large Scale (test_largescale_*)
- **Customers**: 15 customers wanting ~3,840 total  
- **Banks**: 8 banks with 6,300 total
- **Expected**: All customers satisfied, tests concurrent processing
- **Validates**: System scalability, many-process coordination

### Test 4: Big Loan (test_bigloan_*)
- **Customers**: billionaire(25000), student(50), worker(200)
- **Banks**: 4 banks with 12,150 total
- **Expected**: Billionaire drains most banks, others may struggle
- **Validates**: Unequal distribution scenarios

### Test 5: Insufficient Funds (test_insufficient_*)
- **Customers**: 4 customers wanting 1,150 total
- **Banks**: 3 banks with only 230 total  
- **Expected**: Most customers don't get full amounts
- **Validates**: System behavior when demand far exceeds supply

## What to Validate in Output

### 1. Transaction Log Format
```
? customer requests a loan of X dollar(s) from the bank_name bank
$ The bank_name bank approves/denies a loan of X dollar(s) to customer
```

### 2. Message Pairing
- Every `?` (request) should have a corresponding `$` (response)
- No orphaned requests or responses

### 3. Final Report Consistency  
```
** Banking Report **
Customers:
customer: objective X, received Y

Banks:  
bank: original X, balance Y
```

**Critical Validations**:
- `Total received` = `Total loaned` (MUST be equal)
- `received ≤ objective` for each customer
- `balance ≤ original` for each bank
- `original - balance = loaned` for each bank

### 4. Process Termination
- Program should end gracefully with "The financial market is closing for the day..."
- No hanging processes or infinite loops

### 5. Randomness Verification
- Run same test multiple times - results should vary slightly
- Loan amounts should be different in each run
- Bank selection should vary

## Common Issues to Check

### Compilation Errors
```bash
# If you see compilation errors:
erlc money.erl customer.erl bank.erl
# Check syntax, missing commas, parentheses
```

### Runtime Errors
- **Process crashes**: Check message format matching
- **Hanging**: Check timeout values and termination conditions  
- **Wrong totals**: Check arithmetic in update functions

### Logic Validation
- Banks never approve loans > their current funds
- Customers stop when: (received >= objective) OR (no banks left)
- Master process logs ALL transactions

## Performance Testing

### Stress Test (Create Your Own)
```erlang
% Create files with 50+ customers and 20+ banks
% Check system performance and stability
```

### Memory Usage
```bash
# Monitor with Activity Monitor or top while running large tests
```

## Advanced Testing Scenarios

### Edge Cases to Try:
1. **Single customer, single bank** - minimal scenario
2. **Customer wants exactly what bank has** - perfect match
3. **Multiple customers, single bank** - serialization test  
4. **Customer wants $1** - minimum loan test
5. **Customer wants $50** - maximum single loan test

### Error Recovery Testing:
1. **Malformed data files** - add invalid entries
2. **Missing files** - test file error handling
3. **Empty files** - zero customers or banks

## Creating Custom Tests

### Template for Custom Customer File:
```erlang
{customer_name,loan_amount}.
{another_customer,different_amount}.
```

### Template for Custom Bank File:  
```erlang
{bank_name,initial_funds}.
{another_bank,different_funds}.  
```

### Naming Convention:
- Use descriptive names: `test_scenario_c.txt` and `test_scenario_b.txt`
- Customer files end with `_c.txt`
- Bank files end with `_b.txt`

## Expected Behavior Patterns

### Successful Run Indicators:
✅ All processes start without errors  
✅ Transaction log shows realistic loan patterns
✅ Banks gradually run out of money
✅ Customers either get full amounts or exhaust bank options
✅ Final totals match perfectly
✅ Program terminates cleanly

### Red Flags:
❌ Total received ≠ Total loaned
❌ Customer received > objective  
❌ Bank balance < 0
❌ Program hangs indefinitely
❌ No transaction log messages
❌ Compilation errors

Run multiple tests to ensure your implementation is robust! 