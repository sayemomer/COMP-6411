-module(customer).
-export([start/3]).

start(Name, LoanAmount, BankList) ->
    rand:seed(exsss),

    timer:sleep(200),
    
    request_loans(Name, LoanAmount, BankList).

request_loans(Name, RemainingAmount, BankList) ->
    if 
        RemainingAmount =< 0 ->
            master ! {customer_finished, Name};
        length(BankList) == 0 ->

            master ! {customer_finished, Name};
        true ->

            MaxRequest = min(50, RemainingAmount),
            RequestAmount = rand:uniform(MaxRequest),
   
            BankIndex = rand:uniform(length(BankList)),
            SelectedBank = lists:nth(BankIndex, BankList),
        
            SleepTime = rand:uniform(91) + 9,
            timer:sleep(SleepTime),

            master ! {loan_request, Name, SelectedBank, RequestAmount},

            BankPid = whereis(SelectedBank),
            BankPid ! {loan_request, Name, RequestAmount, self()},
            
            receive
                {loan_approved, Amount} ->

                    NewRemaining = RemainingAmount - Amount,
                    master ! {loan_approved, Name, SelectedBank, Amount},
                    request_loans(Name, NewRemaining, BankList);
                    
                {loan_denied, Amount} ->

                    master ! {loan_denied, Name, SelectedBank, Amount},
                    NewBankList = lists:delete(SelectedBank, BankList),
                    request_loans(Name, RemainingAmount, NewBankList)
            after 2000 ->
            
                NewBankList = lists:delete(SelectedBank, BankList),
                request_loans(Name, RemainingAmount, NewBankList)
            end
    end. 