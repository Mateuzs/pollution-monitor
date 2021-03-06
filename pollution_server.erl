
-module(pollution_server).
-author("mateuszzembol").

%%API
-export([start/0, stop/0, crash/0, addStation/2, addValue/4, getMonitor/0, getDeviation/2,
         removeValue/3, getOneValue/3, getDailyMean/2, getStationMean/2]).
-export([init/0]).


start() ->
    register (pollutionServer, spawn_link(pollution_server, init, [])),
    'server started'.

init() ->
    loop(pollution:createMonitor()).



%%% CLIENT

call(Request, Args) ->
    pollutionServer ! {Request, self(), Args},
    receive
        {reply, Reply} -> Reply
    end.

addStation(Name, {PosX,PosY}) -> call(addStation, {Name, {PosX,PosY}}).

addValue(Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Value)->
    call(addValue, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Value}).

removeValue(Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type) ->
    call(removeValue, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type}).

getOneValue(Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type) ->
    call(getOneValue, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type}).

getStationMean(Station, Type) ->
    call(getStationMean, {Station,Type}).

getDailyMean({Year,Month,Day}, Type) ->
    call(getDailyMean, {{Year,Month,Day},Type}).

getDeviation({{Year,Month,Day},{Hour,Minutes,Seconds}}, Type) ->
    call(getDeviation, {{{Year,Month,Day},{Hour,Minutes,Seconds}}, Type}).

getMonitor() -> call(getMonitor, []).

crash() -> call(crash, []).

stop() ->
    call(stop,[]).




%%% SERVER LOOP

loop(Monitor) ->
    receive
        {getMonitor, Pid,  _} ->
            Pid ! {reply, Monitor},
            loop(Monitor);

        {addStation, Pid, {Name, {PosY,PosX}}} ->
            {Result, UptadedMonitor} = 
                pollution:addStation(Name, {PosX, PosY}, Monitor),
            Pid ! {reply, Result},
            loop(UptadedMonitor);
       
        {addValue, Pid, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Value}}->
            {Result, UptadedMonitor} = 
                pollution:addValue( Station,
                                    {{Year, Month, Day},{Hour,Minutes,Seconds}},
                                     Type, Value, Monitor),
                Pid ! {reply, Result},
                loop(UptadedMonitor);
        
        {removeValue, Pid,  {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type}} ->
            {Result,UptadedMonitor} =
                pollution:removeValue(Station,
                                   {{Year, Month, Day},{Hour,Minutes,Seconds}},
                                     Type, Monitor),
                Pid ! {reply, Result},
                loop(UptadedMonitor);
        
        {getOneValue, Pid, {Station,{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type}} ->
            Result =
                pollution:getOneValue(Station,
                                    {{Year, Month, Day},{Hour, Minutes, Seconds}},
                                      Type, Monitor),
                Pid ! {reply, Result},
                loop(Monitor);
        
        {getStationMean, Pid, {Station, Type}} ->
            Result = 
                pollution:getStationMean(Station,Type,Monitor),
                Pid ! {reply, Result},
                loop(Monitor);
        
        {getDailyMean, Pid, {{Year, Month, Day}, Type}} ->
            Result  =
                pollution:getDailyMean({Year, Month, Day}, Type, Monitor),
                Pid ! {reply, Result},
                loop(Monitor);
        
        {getDeviation, Pid, {{{Year, Month, Day},{Hour,Minutes,Seconds}}, Type}} ->
               Result = pollution:getDeviation({{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Monitor),
                Pid ! {reply,Result},
                loop(Monitor);
        
        {crash, Pid, _} ->
                Pid  ! {reply, 'let`s crash!'},
                1/0;

        {stop, Pid, _} ->
            Pid ! {reply, 'server terminated'}
    end.


