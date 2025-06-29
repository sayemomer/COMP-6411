-module(money).
-export([start/1]).

start(Args) ->
    CustomerFile = lists:nth(1, Args),
    BankFile = lists:nth(2, Args),
    {ok, CustomerInfo} = file:consult(CustomerFile),
    {ok, BankInfo} = file:consult(BankFile),
    
    io:format("** The financial market is opening for the day **~n"),
    io:format("Starting transaction log...~n"),

    register(master, self()),

    lists:foreach(fun({BankName, Funds}) ->
        BankPid = spawn(bank, start, [BankName, Funds]),
        register(BankName, BankPid)
    end, BankInfo),

    timer:sleep(100),
    
    lists:foreach(fun({CustomerName, LoanAmount}) ->
        CustomerPid = spawn(customer, start, [CustomerName, LoanAmount, get_bank_names(BankInfo)]),
        register(CustomerName, CustomerPid)
    end, CustomerInfo),

    CustomerData = [{Name, Amount, 0} || {Name, Amount} <- CustomerInfo],
    BankData = [{Name, Amount, Amount} || {Name, Amount} <- BankInfo],
    
    message_loop(CustomerData, BankData, length(CustomerInfo), 0).

get_bank_names(BankInfo) ->
    [BankName || {BankName, _} <- BankInfo].

message_loop(CustomerData, BankData, TotalCustomers, FinishedCustomers) ->
    receive
        {loan_request, Customer, Bank, Amount} ->
            io:format("? ~p requests a loan of ~p dollar(s) from the ~p bank~n", [Customer, Amount, Bank]),
            message_loop(CustomerData, BankData, TotalCustomers, FinishedCustomers);
            
        {loan_approved, Customer, Bank, Amount} ->
            io:format("$ The ~p bank approves a loan of ~p dollar(s) to ~p~n", [Bank, Amount, Customer]),
            UpdatedCustomerData = update_customer_received(CustomerData, Customer, Amount),
            UpdatedBankData = update_bank_funds(BankData, Bank, -Amount),
            message_loop(UpdatedCustomerData, UpdatedBankData, TotalCustomers, FinishedCustomers);
            
        {loan_denied, Customer, Bank, Amount} ->
            io:format("$ The ~p bank denies a loan of ~p dollar(s) to ~p~n", [Bank, Amount, Customer]),
            message_loop(CustomerData, BankData, TotalCustomers, FinishedCustomers);
            
        {customer_finished, Customer} ->
            NewFinishedCount = FinishedCustomers + 1,
            if 
                NewFinishedCount >= TotalCustomers ->
                    print_final_report(CustomerData, BankData),
                    io:format("The financial market is closing for the day...~n");
                true ->
                    message_loop(CustomerData, BankData, TotalCustomers, NewFinishedCount)
            end;
            
        _ ->
            message_loop(CustomerData, BankData, TotalCustomers, FinishedCustomers)
    after 5000 ->
        print_final_report(CustomerData, BankData),
        io:format("The financial market is closing for the day...~n")
    end.

update_customer_received(CustomerData, Customer, Amount) ->
    [{Name, Objective, if Name == Customer -> Received + Amount; true -> Received end} || 
     {Name, Objective, Received} <- CustomerData].

update_bank_funds(BankData, Bank, AmountChange) ->
    [{Name, Original, if Name == Bank -> Current + AmountChange; true -> Current end} || 
     {Name, Original, Current} <- BankData].

print_final_report(CustomerData, BankData) ->
    io:format("** Banking Report **~n"),
    io:format("Customers:~n"),
    
    TotalObjective = lists:foldl(fun({Name, Objective, Received}, {AccObj, AccRec}) ->
        io:format("~p: objective ~p, received ~p~n", [Name, Objective, Received]),
        {AccObj + Objective, AccRec + Received}
    end, {0, 0}, CustomerData),
    
    {TotalObj, TotalRec} = TotalObjective,
    io:format("----Total: objective ~p, received ~p~n", [TotalObj, TotalRec]),
    
    io:format("Banks:~n"),
    TotalBanking = lists:foldl(fun({Name, Original, Current}, {AccOrig, AccLoaned}) ->
        Loaned = Original - Current,
        io:format("~p: original ~p, balance ~p~n", [Name, Original, Current]),
        {AccOrig + Original, AccLoaned + Loaned}
    end, {0, 0}, BankData),
    
    {TotalOrig, TotalLoaned} = TotalBanking,
    io:format("----Total: original ~p, loaned ~p~n", [TotalOrig, TotalLoaned]). 