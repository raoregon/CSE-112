/*
Roberto Oregon
raoregon@ucsc.edu

Programming Partner: Jeremiah Chen

(a) To compute the distance between airports, use the haversine formula. The database 
contains degrees and minutes, which must be converted to radians. The result must be 
converted to miles.

(b) Planes fly at a constant speed of 500 miles per hour and always using great circle 
paths, so the arrival time can be computed from the departure time.

(c) A flight transfer always takes 30 minutes, so during a transfer at a hub, the 
departure of a connecting flight must be at least 30 minutes later than the arrival of 
the incoming flight.

(d) There are no overnight trips. The complete trip departs and arrives in the same day, 
although the final arrival time may be shown as greater than 23:59.
*/

% Airports: (three letter code, city name, north latitude in degree minutes,
% west longitude in degree minutes)

airport( atl, 'Atlanta         ', degmin(  33,39 ), degmin(  84,25 ) ).
airport( bos, 'Boston-Logan    ', degmin(  42,22 ), degmin(  71, 2 ) ).
airport( chi, 'Chicago         ', degmin(  42, 0 ), degmin(  87,53 ) ).
airport( den, 'Denver-Stapleton', degmin(  39,45 ), degmin( 104,52 ) ).
airport( dfw, 'Dallas-Ft.Worth ', degmin(  32,54 ), degmin(  97, 2 ) ).
airport( lax, 'Los Angeles     ', degmin(  33,57 ), degmin( 118,24 ) ).
airport( mia, 'Miami           ', degmin(  25,49 ), degmin(  80,17 ) ).
airport( nyc, 'New York City   ', degmin(  40,46 ), degmin(  73,59 ) ).
airport( sea, 'Seattle-Tacoma  ', degmin(  47,27 ), degmin( 122,17 ) ).
airport( sfo, 'San Francisco   ', degmin(  37,37 ), degmin( 122,23 ) ).
airport( sjc, 'San Jose        ', degmin(  37,22 ), degmin( 121,56 ) ).


% Departure times: (departing, arriving, military time)

flight( bos, nyc, time( 7,30 ) ).
flight( dfw, den, time( 8, 0 ) ).
flight( atl, lax, time( 8,30 ) ).
flight( chi, den, time( 8,45 ) ).
flight( mia, atl, time( 9, 0 ) ).
flight( sfo, lax, time( 9, 0 ) ).
flight( sea, den, time( 10, 0 ) ).
flight( nyc, chi, time( 11, 0 ) ).
flight( sea, lax, time( 11, 0 ) ).
flight( den, dfw, time( 11,15 ) ).
flight( sjc, lax, time( 11,15 ) ).
flight( atl, lax, time( 11,30 ) ).
flight( atl, mia, time( 11,30 ) ).
flight( chi, nyc, time( 12, 0 ) ).
flight( lax, atl, time( 12, 0 ) ).
flight( lax, sfo, time( 12, 0 ) ).
flight( lax, sjc, time( 12, 15 ) ).
flight( nyc, bos, time( 12,15 ) ).
flight( bos, nyc, time( 12,30 ) ).
flight( den, chi, time( 12,30 ) ).
flight( dfw, den, time( 12,30 ) ).
flight( mia, atl, time( 13, 0 ) ).
flight( sjc, lax, time( 13,15 ) ).
flight( lax, sea, time( 13,30 ) ).
flight( chi, den, time( 14, 0 ) ).
flight( lax, nyc, time( 14, 0 ) ).
flight( sfo, lax, time( 14, 0 ) ).
flight( atl, lax, time( 14,30 ) ).
flight( lax, atl, time( 15, 0 ) ).
flight( nyc, chi, time( 15, 0 ) ).
flight( nyc, lax, time( 15, 0 ) ).
flight( den, dfw, time( 15,15 ) ).
flight( lax, sjc, time( 15,30 ) ).
flight( chi, nyc, time( 18, 0 ) ).
flight( lax, atl, time( 18, 0 ) ).
flight( lax, sfo, time( 18, 0 ) ).
flight( nyc, bos, time( 18, 0 ) ).
flight( sfo, lax, time( 18, 0 ) ).
flight( sjc, lax, time( 18,15 ) ).
flight( atl, mia, time( 18,30 ) ).
flight( den, chi, time( 18,30 ) ).
flight( lax, sjc, time( 19,30 ) ).
flight( lax, sfo, time( 20, 0 ) ).
flight( lax, sea, time( 22,30 ) ).

/*
This is what our goal is pointing to, takes in A value for departing flight and
B for arriving flight, calls function fly to calculate flight
*/
main :- read(Depart),read(Arrive), fly(Depart,Arrive).

fly(Depart, Arrive) :-
    listPath(Depart, Arrive, [Depart], List, _),
    print_trip(List),
    true.

% helper function that takes in two airports and produces their haversine distance
distanceCalculate(Airport, Airport2, Distance) :-
    airport(Airport, _, Lat1, Lon1),
    airport(Airport2, _, Lat2, Lon2),
    haversineDistance(Lon1, Lat1, Lon2, Lat2, Distance).
    
% Calculate Haversine Distance. Formula has been provided by Dr. Math Forum:
% http://mathforum.org/library/drmath/view/51879.html
haversineDistance(Lon1,Lat1, Lon2, Lat2, ValueD):-
    radianConversion(Lon1, Lon1Value),
    radianConversion(Lon2, Lon2Value),
    radianConversion(Lat1, Lat1Value),
    radianConversion(Lat2, Lat2Value),
    Dlon is Lon2Value - Lon1Value,
    Dlat is Lat2Value - Lat1Value,
    EarthRadius is 3956,
    ValueA is sin(Dlat / 2)** 2 + cos(Lat1Value) * cos(Lat2Value) * sin(Dlon / 2)** 2,
    ValueC is 2 * atan2(sqrt(ValueA), sqrt(1 - ValueA)),
    ValueD is EarthRadius * ValueC.

radianConversion(degmin(Degrees, Minutes), Rads) :-
     Degs is (Degrees + Minutes / 60),
     Rads is (Degs * pi / 180).

militaryTimeConversion(time(Hours, Minutes), MilitaryTime) :-
    MilitaryTime is Hours + Minutes / 60.

militaryTimeReversal(MilitaryTime, Hours, Minutes) :-
    Hours is floor(MilitaryTime * 60) // 60,
    Minutes is floor(MilitaryTime * 60) mod 60.
    
/*
Takes in our Depart and Arrive values and checks to see if there is an immediate flight.
If no flight is found, it looks for other possible connections, and adds their transfers
to the list.
*/
listPath(Transfer, FinDestination, Visited, [[Transfer, DepartMilitaryTime, ArriveTime] | List], DepartTime) :-
    flight(Transfer, FinDestination, DepartTime),
    not(member(FinDestination, Visited)),
    militaryTimeConversion(DepartTime, DepartMilitaryTime),
    distanceCalculate(Transfer, FinDestination, Distance),
    TravelTime is Distance / 500,
    ArriveTime is DepartMilitaryTime + TravelTime,
    listPath(FinDestination, FinDestination, [FinDestination | Visited], List, _).

listPath(Transfer, Transfer, _, [Transfer], _).

listPath(Transfer, FinDestination, Visited,[[Transfer, DepartMilitaryTime, ArriveTime] | List], DepartTime) :-
    flight(Transfer, Next, DepartTime),
    not(member(Next, Visited)),
    militaryTimeConversion(DepartTime, DepartMilitaryTime),
    distanceCalculate(Transfer, Next, Distance),
    TravelTime is Distance / 500,
    ArriveTime is DepartMilitaryTime + TravelTime,
    flight(Next, _, NextDepartTime),
    militaryTimeConversion(NextDepartTime, NextDepartMilitaryTime),
    TransitTime is NextDepartMilitaryTime - ArriveTime - 0.5,
    TransitTime >= 0,
    listPath(Next, FinDestination, [Next | Visited], List, NextDepartTime).

print_trip([[Depart, DepartTime, ArriveTime], Arrive | []]) :-
    airport(Depart, DepartName, _, _), 
    airport(Arrive, ArriveName, _, _),
    upcase_atom(Depart, UpperD),
    upcase_atom(Arrive, UpperA),
    militaryTimeReversal(DepartTime, DHour, DMin),
    militaryTimeReversal(ArriveTime, AHour, AMin),
    format( "depart  ~s  ~s~26|  ~`0t~d~29|:~`0t~d~32|", [UpperD,DepartName, DHour, DMin]),
    nl,
    format( "arrive  ~s  ~s~26|  ~`0t~d~29|:~`0t~d~32|", [UpperA,ArriveName, AHour, AMin]),
    nl.

print_trip([[Depart, DepartTime, ArriveTime], 
	[Transit, TransitDepartureTime, TransitArrivalTime] | Arrive]) :-
    airport(Depart, DepartName, _, _), 
    airport(Transit, ArriveName, _, _),
    upcase_atom(Depart, UpperD),
    upcase_atom(Transit, UpperA),
    militaryTimeReversal(DepartTime, DHour, DMin),
    militaryTimeReversal(ArriveTime, AHour, AMin),
    format( "depart  ~s  ~s~26|  ~`0t~d~29|:~`0t~d~32|", [UpperD,DepartName, DHour, DMin]),
    nl,
    format( "arrive  ~s  ~s~26|  ~`0t~d~29|:~`0t~d~32|", [UpperA,ArriveName, AHour, AMin]),
    nl,
    print_trip([[Transit, TransitDepartureTime, TransitArrivalTime] | Arrive]).
  
