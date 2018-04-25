-module(pollution_server_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).


create_monitor_test()  -> 
    ?assertEqual('server started', pollution_server:start()).

get_monitor_test() ->
            ?assertEqual(#{}
                        , pollution_server:getMonitor()).

add_station_test() ->
    ?assertEqual(ok,
                 pollution_server:addStation("aleja",{23,23})).

add_value_test() ->
    ?assertEqual(ok,
                 pollution_server:addValue({234,3984},{{2018,4,22},{15,23,11}},'pm 10',23)),
    
    ?assertEqual("error: This value already Exists!",
                 pollution_server:addValue({234,3984},{{2018,4,22},{15,23,11}},'pm 10',23)).

add_value2_test() ->
    ?assertEqual(ok,
                 pollution_server:addValue("aleja",{{2018,4,22},{15,23,11}},'pm 10',23)),

    
    ?assertEqual("error: This value already Exists!",
                 pollution_server:addValue("aleja",{{2018,4,22},{15,23,11}},'pm 10',23)).

remove_value_test() ->
        
    ?assertEqual(ok,
                    pollution_server:removeValue("aleja",{{2018,4,22},{15,23,11}},'pm 10')),
    ?assertEqual(ok,  
                    pollution_server:removeValue({234,3984},{{2018,4,22},{15,23,11}},'pm 10')).

    
get_one_value_test() ->

            pollution_server:addValue("aleja",{{2018,4,22},{15,23,11}},'pm 10',23),
    
            ?assertEqual({value,23}, pollution_server:getOneValue("aleja",{{2018,4,22},{15,23,11}},'pm 10')),
    
            ?assertEqual("there isn't such measurement!", pollution_server:getOneValue("aleja",{{2018,4,22},{15,23,11}},'pm 2')).
    
get_one_value2_test() ->
            
            pollution_server:addValue({11,111},{{2018,4,22},{15,23,11}},'pm 10',23),
    
            ?assertEqual({value,23}, pollution_server:getOneValue({11,111},{{2018,4,22},{15,23,11}},'pm 10')).
            
    
get_station_mean_test() ->
    
    
        pollution_server:addValue("aleja",{{2018,4,22},{15,24,11}},'pm 10',23),
        pollution_server:addValue("aleja",{{2018,4,22},{15,25,11}},'pm 10',26),
        pollution_server:addValue("aleja",{{2018,4,22},{15,26,11}},'pm 10',21),
    
        ?assertEqual(23.25 , pollution_server:getStationMean("aleja", 'pm 10')).
    
get_daily_mean_test() ->
    
        pollution_server:addValue("ruczaj",{{2018,4,22},{14,24,11}},'pm 10',34),
        pollution_server:addValue("ruczaj",{{2018,4,22},{18,25,11}},'pm 10',31),
        pollution_server:addValue("ruczaj",{{2018,4,22},{11,26,11}},'pm 10',37),
        
    
        ?assertEqual(27.25 , pollution_server:getDailyMean({2018,4,22}, 'pm 10')).

get_deviation_test() ->

        ?assertEqual(1.6, pollution_server:getDeviation({{2018,4,22},{15,24,11}},'pm 10')).    

server_stop_test() ->
    ?assertEqual('server terminated'
                , pollution_server:stop()).
