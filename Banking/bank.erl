-module(bank).
-export([start/2]).

start(Name, InitialFunds) ->

    bank_loop(Name, InitialFunds).

bank_loop(Name, CurrentFunds) ->
    receive
        {loan_request, Customer, Amount, CustomerPid} ->

            if 
                Amount =< CurrentFunds ->

                    NewFunds = CurrentFunds - Amount,

                    bank_loop(Name, NewFunds);
                true ->
                
                    CustomerPid ! {loan_denied, Amount},
                    bank_loop(Name, CurrentFunds)
            end;
            
        stop ->

            ok;
            
        _ ->

            bank_loop(Name, CurrentFunds)
    after 10000 ->
    
        ok
    end. 