-module(state_container).
-behaviour(gen_server).



-export([start_link/1, stop/0, init/1, getState/0, updateState/1,
         handle_call/3, handle_cast/2, terminate/2]).



start_link(InitState) ->
    gen_server:start_link( {local, state_container},
                            state_container,
                            InitState, 
                            []).


init(_InitState) ->
    io:format("state container started.. ~n"),
    {ok, pollution:createMonitor()}.


%%% API %%%

getState() ->
    gen_server:call(state_container, {getState}).

updateState(State) ->
    gen_server:cast(state_container, {updateState, {State}}).

stop() ->
    gen_server:cast(state_container, {stop}).

%%% CALLBACKS %%%

handle_call({getState}, _From, State) ->
    {reply, State, State}.

handle_cast({updateState, {UpdatedState}},  _State) ->
    {noreply, UpdatedState};

handle_cast({stop}, State) ->
    {stop, shutdown, State}.


terminate(_Reason, _State) ->
    io:format("state container terminates.. ~n"),
    ok.


