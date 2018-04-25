-module(pollution_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

create_monitor_test()  -> 
    ?assertEqual(#{}, pollution:createMonitor()).

add_station_test() ->
    M = #{},
    ?assertEqual({ok, #{"aleja" => {station,"aleja",{location,23,23}}}},
                 pollution:addStation("aleja",{23,23},M )).

add_value_test() ->
    M = #{},
    ?assertEqual({ok,#{{measurement,{station,undefined,{location,111,435}},
                 {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}}},
                 pollution:addValue({111,435},{{2018,4,22},{15,23,11}},'pm 10',23,M)),
    
    M1 = #{{measurement,{station,undefined,{location,111,435}},
    {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}},          
    ?assertEqual({"error: This value already Exists!",#{{measurement,{station,undefined,{location,111,435}},
                 {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}}},
                 pollution:addValue({111,435},{{2018,4,22},{15,23,11}},'pm 10',23,M1)).

add_value2_test() ->
    M = #{},
    ?assertEqual({ok,#{{measurement,{station,"aleja",{location,undefined,undefined}},
                 {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}}},
                 pollution:addValue("aleja",{{2018,4,22},{15,23,11}},'pm 10',23,M)),

    M1 = #{{measurement,{station,"aleja",{location,undefined,undefined}},
                 {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}},
    ?assertEqual({"error: This value already Exists!",#{{measurement,{station,"aleja",{location,undefined,undefined}},
                 {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}}},
                 pollution:addValue("aleja",{{2018,4,22},{15,23,11}},'pm 10',23,M1)).


remove_value_test() ->
        M1 = #{{measurement,{station,"aleja",{location,undefined,undefined}},
        {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}},
        
        ?assertEqual( {ok,#{}},
        pollution:removeValue("aleja",{{2018,4,22},{15,23,11}},'pm 10',M1)),
        
        M2 = #{{measurement,{station,"aleja",{location,undefined,undefined}},
        {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}},

        ?assertEqual({ok,#{{measurement,{station,"aleja",{location,undefined,undefined}},
                        {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}}},
                        pollution:removeValue("a",{{2018,4,22},{15,23,11}},'pm 10',M2)),

        M3 = #{{measurement,{station,undefined,{location,234,3984}},
        {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}},

        ?assertEqual( {ok,#{}},
        pollution:removeValue({234,3984},{{2018,4,22},{15,23,11}},'pm 10',M3)).



get_one_value_test() ->

        M = #{{measurement,{station,"aleja",{location,undefined,undefined}},
        {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}},

        ?assertEqual({value,23}, pollution:getOneValue("aleja",{{2018,4,22},{15,23,11}},'pm 10', M)),

        ?assertEqual("there isn't such measurement!", pollution:getOneValue("aleja",{{2018,4,22},{15,23,11}},'pm 2', M)).

get_one_value2_test() ->
        M = #{{measurement,{station,undefined,{location,11,111}},
        {date,2018,4,22,15,23,11}, 'pm 10'} => {value,23}},

        ?assertEqual({value,23}, pollution:getOneValue({11,111},{{2018,4,22},{15,23,11}},'pm 10', M)).
        

get_station_mean_test() ->

    M = #{{measurement,{station,"aleja",{location,undefined,undefined}},
    {date,2018,4,22,15,24,11}, 'pm 10'} => {value,23},
    {measurement,{station,"aleja",{location,undefined,undefined}},
    {date,2018,4,22,15,25,11}, 'pm 10'} => {value,26},
    {measurement,{station,"aleja",{location,undefined,undefined}},
    {date,2018,4,22,15,26,11}, 'pm 10'} => {value,21}},

    ?assertEqual(23.333333333333332 , pollution:getStationMean("aleja", 'pm 10',M)).

get_daily_mean_test() ->

        M = #{{measurement,{station,"aleja",{location,undefined,undefined}},
        {date,2018,4,22,10,24,11}, 'pm 10'} => {value,23},
        {measurement,{station,"aleja",{location,undefined,undefined}},
        {date,2018,4,22,11,25,11}, 'pm 10'} => {value,26},
        {measurement,{station,"aleja",{location,undefined,undefined}},
        {date,2018,4,22,15,26,11}, 'pm 10'} => {value,21},
        {measurement,{station,"ruczaj",{location,undefined,undefined}},
        {date,2018,4,22,14,24,11}, 'pm 10'} => {value,34},
        {measurement,{station,"ruczaj",{location,undefined,undefined}},
        {date,2018,4,22,15,25,11}, 'pm 10'} => {value,31},
        {measurement,{station,"ruczaj",{location,undefined,undefined}},
        {date,2018,4,22,16,26,11}, 'pm 10'} => {value,37}},

        ?assertEqual(28.666666666666668 , pollution:getDailyMean({2018,4,22}, 'pm 10',M)).
