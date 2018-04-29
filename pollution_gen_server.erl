-module(pollution_gen_server).
-behaviour(gen_server).

-export([start/0, stop/0, addStation/2, addValue/4, getOneValue/3, crasher/0, getMonitor/0,
        removeValue/3, getStationMean/2, getDailyMean/2, getDeviation/2]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, terminate/2]).


start_link(InitValue) -> 
    gen_server:start_link(
        {local, pollution_gen_server}, 
        pollution_gen_server,
         InitValue,
         []).

init(_InitValue) ->
    io:format("The server has started :)~n") ,
    {ok, state_container:getState()}.

%%% USER API %%%

start() ->
    server_supervisor:start().

addStation(Name, {PosX,PosY}) ->
    gen_server:cast(pollution_gen_server, {addStation, {Name,{PosX,PosY}}}).

addValue(Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Value)->
    gen_server:cast(pollution_gen_server, {addValue, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Value}}).

removeValue(Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type) ->
    gen_server:cast(pollution_gen_server, {removeValue, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type}}).

getOneValue(Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type) ->
    gen_server:call(pollution_gen_server, {getOneValue, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type}}).

getStationMean(Station,Type) ->
    gen_server:call(pollution_gen_server, {getStationMean, {Station,Type}}).

getDailyMean({Year,Month,Day}, Type) ->
    gen_server:call(pollution_gen_server, {getDailyMean, {{Year,Month,Day},Type}}).

getDeviation({{Year,Month,Day},{Hour,Minutes,Seconds}}, Type) ->
    gen_server:call(pollution_gen_server, {getDeviation, {{{Year,Month,Day},{Hour,Minutes,Seconds}}, Type}}).

getMonitor() ->
    gen_server:call(pollution_gen_server, {getMonitor}).

crasher() ->
    gen_server:cast(pollution_gen_server, {crasher}).

stop() ->
    io:format("server and state container terminate.. ~n"),
    exit(whereis(varSupervisor), normal).

%%% CALLBACKS %%%

handle_cast({addStation, {Name,{PosX,PosY}}}, Monitor) ->
        {_, UpdatedMonitor} = pollution:addStation(Name, {PosX,PosY}, Monitor),
        {noreply, UpdatedMonitor};

handle_cast({addValue, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Value}}, Monitor) ->
        {_, UpdatedMonitor} = pollution:addValue(Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Value, Monitor),
        {noreply, UpdatedMonitor};

handle_cast({removeValue, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type}}, Monitor) ->
        {_, UpdatedMonitor} = pollution:removeValue(Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Monitor),
        {noreply, UpdatedMonitor};

handle_cast({stop}, Monitor) ->
    {stop, shutdown, Monitor};

handle_cast({crasher}, Monitor) ->
     io:format("we're gonna crash, Bejbe!!!! ~n"),
     1 / 0 ,
     {noreply, Monitor}.

handle_call({getOneValue, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type}}, _From,  Monitor) ->
    {reply, pollution:getOneValue(Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Monitor), Monitor};

handle_call({getStationMean, {Station, Type}}, _From, Monitor) ->
    {reply, pollution:getStationMean(Station,Type,Monitor) ,Monitor};

handle_call({getDailyMean, {{Year,Month,Day},Type}}, _From, Monitor) ->
    {reply, pollution:getDailyMean({Year,Month,Day}, Type, Monitor), Monitor};

handle_call({getDeviation, {{{Year,Month,Day},{Hour,Minutes,Seconds}}, Type}}, _From,  Monitor) ->
    {reply, pollution:getDeviation({{Year,Month,Day},{Hour,Minutes,Seconds}}, Type, Monitor), Monitor};

handle_call({getMonitor}, _From, Monitor) ->
    {reply, Monitor, Monitor}.


terminate(_Reason, State) ->
    io:format("server terminates.. ~n"),
    state_container:updateState(State),
    ok.


