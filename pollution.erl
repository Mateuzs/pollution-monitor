
-module(pollution).
-author("mateuszzembol").

%%% API %%%
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4,
         getOneValue/4, getDailyMean/3, getStationMean/3]).

%%% RECORDS %%%

-record(location, {posx, posy}).
-record(date, {year, month, day, hour, minutes, seconds}).
-record(station, {name, location = #location{}}).
-record(measurement, {station = #station{}, date = #date{}, type}).
-record(value, {value}).



%%% MAIN FUNCTIONS %%%

createMonitor() -> #{}.


addStation(Name, {PosX, PosY}, Monitor) ->

  Station = #station{name = Name, location = #location{posx=PosX, posy=PosY}},

  case  containsStation(Name, PosX, PosY, Monitor) of
        
        true -> {"error: This station already Exists!", Monitor};
        false -> {ok, Monitor#{Name => Station}}
  end.


addValue({PosX,PosY}, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Value, Monitor)->

    Measurement = createMeasurement({PosX,PosY}, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type),
    Result = #value{value = Value},

    case maps:is_key(Measurement,Monitor) of
        true -> {"error: This value already Exists!", Monitor};
        false -> {ok, Monitor#{Measurement => Result}}
      end;


addValue(Name, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Value, Monitor)->

    Measurement = createMeasurement(Name, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type),
    Result = #value{value = Value},

    case maps:is_key(Measurement,Monitor) of
      true -> {"These value already Exists!", Monitor};
      false -> {ok, Monitor#{Measurement => Result}}
    end.
       

removeValue({PosX,PosY}, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Monitor)->
    Key = createMeasurement({PosX,PosY}, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type),
    {ok, maps:remove(Key, Monitor)};


removeValue(Name, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Monitor) ->
    Key = createMeasurement(Name, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type),
    {ok, maps:remove(Key, Monitor)}.


getOneValue({PosX,PosY}, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Monitor) ->
    Key = createMeasurement({PosX,PosY}, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type),
      maps:get(Key,Monitor);


getOneValue(Name, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type, Monitor) ->
    Key = createMeasurement(Name, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type),
     maps:get(Key,Monitor, "there isn't such measurement!").



getStationMean({PosX,PosY}, Type, Monitor) ->
  Filter = fun (Measurement, Value, {Sum,Count}) when
         Measurement#measurement.station#station.location#location.posx == PosX andalso
         Measurement#measurement.station#station.location#location.posy == PosY andalso
         Measurement#measurement.type == Type 
                                -> {Sum + Value#value.value, Count +1} end, 
  
  {Sum,Count} = maps:fold(Filter, {0,0}, Monitor),
    Sum / Count;


getStationMean(Name, Type, Monitor) ->
  Filter = fun (Measurement, Value, {Sum,Count}) 
          when Measurement#measurement.station#station.name == Name
          andalso Measurement#measurement.type == Type 
                                            -> {Sum + Value#value.value, Count +1};
        
          (_, _, {Sum,Count}) -> {Sum,Count} end,
  
  case maps:fold(Filter, {0,0}, Monitor) of
    {0,0} -> "There is no measurements!"; 
    {Sum,Count} -> Sum / Count
  end.


getDailyMean({Year, Month, Day}, Type, Monitor) ->
  Filter = fun (Measurement, Value, {Sum,Count}) when
         Measurement#measurement.date#date.year == Year andalso
         Measurement#measurement.date#date.month == Month andalso
         Measurement#measurement.date#date.day == Day andalso
         Measurement#measurement.type == Type 
                              -> {Sum + Value#value.value, Count +1};
          (_, _, {Sum,Count}) -> {Sum,Count} end,
  
   case maps:fold(Filter, {0,0}, Monitor) of
    {0,0} -> "There is no measurements!";
    {Sum,Count} -> Sum / Count
  end.



%%% FILTER FUNCTIONS %%%

containsStation(Name, PosX, PosY, Monitor) ->

  case maps:is_key(Name, Monitor) of
    true -> true;
    false -> 

        Filter = fun (_,Station) ->
          (Station#station.location#location.posy==PosY andalso
           Station#station.location#location.posx==PosX ) end,

      Filtered = maps:filter(Filter, Monitor),
      case maps:size(Filtered) of
        0 -> false;
        _ -> true
      end
  end.



%%% ADDITIONAL FUNCTIONS %%%

createMeasurement({PosX,PosY}, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type) ->

    #measurement{
     station = # station{ location = #location{posx = PosX,posy = PosY}},
     date = #date{
       year = Year,
        month = Month,
        day = Day,
        hour = Hour,
        minutes = Minutes,
        seconds = Seconds},
      type = Type};


createMeasurement(Name, {{Year, Month, Day},{Hour,Minutes,Seconds}}, Type) ->

    #measurement{
      station = #station{name = Name},
      date = #date{
        year = Year,
        month = Month,
        day = Day,
        hour = Hour,
        minutes = Minutes,
        seconds = Seconds},
      type = Type }.

