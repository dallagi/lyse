-module(evserv).
-compile(export_all).

-record(state, {events, clients}).
-record(event, {name="", description="", pid, timeout={{1970,1,1},{0,0,0}}}).

start() ->
    register(?MODULE, Pid=spawn(?MODULE, init, [])),
    Pid.
 
start_link() ->
    register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
    Pid.
 
terminate() ->
    ?MODULE ! shutdown.

add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive {Ref, Msg} -> Msg
    after 5000 -> {error, timeout}
    end.

add_event2(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
        {Ref, {error, Reason}} -> erlang:error(Reason);
        {Ref, Msg} -> Msg
    after 5000 ->
        {error, timeout}
    end.

cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive {Ref, ok} -> ok
    after 5000 -> {error, timeout}
    end.

subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! {self(), Ref, {subscribe, Pid}},
    receive
        {Ref, ok} -> {ok, Ref};
        {'DOWN', Ref, process, _Pid, Reason} -> {error, Reason}
    after 5000 -> {error, timeout}
    end.
 
init() ->
    loop(#state{events=orddict:new(), clients=orddict:new()}).

loop(State = #state{}) ->
    receive
        {Pid, MsgRef, {subscribe, Client}} ->
            Ref = erlang:monitor(process, Client),
            NewClients = orddict:store(Ref, Client, State#state.clients),
            Pid ! {MsgRef, ok},
            loop(State#state{clients=NewClients});

        {Pid, MsgRef, {cancel, Name}} ->
            Events = case orddict:find(Name, State#state.events) of
                        {ok, E} ->
                            event:cancel(E#event.pid),
                            orddict:erase(Name, State#state.events);
                        error ->
                            State#state.events
                     end,
            Pid ! {MsgRef, ok},
            loop(State#state{events=Events});

        {done, Name} ->
            case orddict:find(Name, State#state.events) of
                {ok, E} ->
                    send_to_clients({done, E#event.name, E#event.description}, State#state.clients),
                    NewEvents = orddict:erase(Name, State#state.events),
                    loop(State#state{events=NewEvents});
                error ->
                    loop(State)
            end;

        shutdown ->
            exit(shutdown);

        {'DOWN', Ref, process, _Pid, _Reason} ->
            loop(State#state{clients=orddict:erase(Ref, State#state.clients)});

        code_change ->
            ?MODULE:loop(State);

        Unknown ->
            io:format("Unknown message: ~p~n",[Unknown]),
            loop(State)           
    end.

send_to_clients(Msg, ClientDict) ->
    orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, ClientDict).

valid_datetime({Date,Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
        error:function_clause -> %% not in {{Y,M,D},{H,Min,S}} format
        false
    end;
valid_datetime(_) -> false.
    
valid_time({H,M,S}) -> valid_time(H,M,S).
valid_time(H,M,S) when H >= 0, H < 24, M >= 0, M < 60, S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.
