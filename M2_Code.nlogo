;ESE 420/520 Midterm 2
;Zhiyang Chen, Bari Gordon, Meiqing Li
;Prof. Barry Silverman
;November 9th, 2017

globals
[
  ;; destination value specific variables
  max-money ;; max amount of money a turtle can hold
  max-family ;; max amount of family time a turtle can have
  max-education ;; max amount of education a turtle can have
  max-materialism ;; max amount of materialism each turtle can exhibit

  ;; in future models we plan to have separate acceleration for AV and HV
  acceleration             ;; the constant that controls how much a car speeds up or slows down by if
  phase                    ;; keeps track of the phase

  num-parking-spots        ;; tracks total number of parking spots

  ;; lists that hold parking spots for each location
  list-parking-house
  list-parking-work
  list-parking-school
  list-parking-shop
  list-parking-spot

  ;; number of people using AVs at each location
  AV-people-count-house
  AV-people-count-work
  AV-people-count-school
  AV-people-count-shop

  ;; variables for agents
  total-num-cars
  num-HVs
  num-AVs
  num-HV-people
  num-AV-people

  ;; amount of HVs that one AV can replace
  replacement-rate

  ;; amount of goals successfully met
  num-goals-met

  ;; patch agentsets
  intersections ;; agentset containing the patches that are intersections
  cross-walks   ;; agentset containing the patches for HV decisions at intersection
  roads         ;; agentset containing the patches that are roads
  work-road     ;; agentset containing the patches of road related to work
  house-road    ;; agentset containing the patches of road related to house
  school-road   ;; agentset containing the patches of road related to school
  shop-road     ;; agentset containing the patches of road related to shop
  parking-roads ;; agentset containing the patches that are roads for parking spots
  side-walks    ;; agentset containing the patches that are sidewalks
  work          ;; the place/parkings to go to or leave from
  house         ;; the place/parkings to go to or leave from
  school        ;; the place/parkings to go to or leave from
  shop          ;; the place/parkings to go to or leave from
  goal-candidates ;; agentset containing the patches of 4 locations drop off and pickup spot

  p-cross-walks ;;agentset containing the patches of roads that pedestrians cross
]

;; AVs, HVs and people are both breeds of turtle
breed [ AVs AV ]  ;automous vehicle
breed [ HVs HV ]  ;human-operated vehicle
breed [ People Person] ; pedestrians walking on side walk

breed [ accidents accident ] ;; creates an accident breed

accidents-own [
  clear-in ; how many ticks before an accident is cleared
]

turtles-own
[
  speed      ;; the speed of the turtle
  up-car?    ;; true if the turtle moves downwards, false if it turns right/left
  wait-time  ;; the amount of time since the last time a turtle has moved
  count-down ;; the time it spends in the parking spot
  from       ;; inital spot (for AVs: initial pick-up location; for HVs: the location they leave parking spot from)
  goto       ;; destination (goals)
  on?        ;; if the car is running
  passenger? ;; true if there is passenger in the AV

  ;; specific to destination values (for HVs and Pedestrian)
  money ;; money increases when go to work and decreases when shop or school
  family ;; family increases when go home, decreases at work and shop
  education ;; education increases when go to school, decreases when shop
  materialism ;; increases when shop, decrease at home and school

  ;; adding emotion to our model, the ideal emotional state is 0 (neutral)
  EMOTION_LEVEL ; level of emotions for pedestrians and HVs
  risk-factor ; for HV and Pedestrians
  jaywalk     ; true if HVs and pedestrian jaywalk

  ;; distance to each location from current spot
  d_work
  d_school
  d_house
  d_shop

  list-foes ; list of foes for each agent

  time-to-dest-incr-AV ; time that an AV currently spend on the way to destination (changes per tick)
  time-to-dest-incr-HV ; time that a HV currently spend on the way to destination (changes per tick)
  time-to-dest-incr-people ; time that a pedestrian currently spend on the way to destination (changes per tick)

  time-to-dest-AV ; time it takes for an AV to reach new destination
  time-to-dest-HV ; time it takes for an HV to reach new destination
  time-to-dest-people ; time it takes for a pedestrian to reach new destination
]

patches-own
[
  intersection?   ;; true if the patch is at the intersection of two roads
  green-light-up? ;; true if the green light is above the intersection,, otherwise, false.
  my-phase        ;; the phase for the intersection. -1 for non-intersection patches.
  auto?           ;; whether or not this intersection will switch automatically.

  ;; for parking lot
  house?          ;; true is this is a parking spot for house
  work?           ;; true is this is a parking spot for work
  school?         ;; true is this is a parking spot for school
  shop?           ;; true is this is a parking spot for shop
  occupied?       ;; whether the parking spot is occupied

  light-here?     ;; true if this patch is before traffic light
]

;;;;;;;;;;;;;;;;;;;;;;
;; Setup Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Initialize the display by giving the global and patch variables initial values.
;; Create num-cars of turtles if there are enough road patches for one turtle to
;; be created per road patch.

to setup
  clear-all
  setup-globals  ;; initialize global variables
  setup-patches  ;; ask the patches to draw themselves and set up a few variables

  ;; Make an agentset of all patches where there can be one of the four places
  ;; those patches are house, work, shop and school
  set goal-candidates (patch-set house work shop school)

  ;; warning if there are too many cars for the road
  if (num-AVs + num-HVs > count roads) [
    user-message (word
      "There are too many cars for the amount of "
      "road.  Either increase the amount of roads "
      "by increasing the GRID-SIZE-X or "
      "GRID-SIZE-Y sliders, or decrease the "
      "number of cars by lowering the NUM-CAR slider.\n"
      "The setup has stopped.")
    stop
  ]

  ;; create Pedestrians
  create-People num-pedestrians
  [
    set shape "person" ; set up the basic figure for pedestrian
    set color pink
    set size 0.9

    set speed 0        ; set initial speed to be 0
    set wait-time 0    ; set initial wait time to be 0
    set on? true       ; set on? to be true

    ;; assigns the risk-factor to pedestrian according to probability distribution (takes into account gender and age)
    ;; the probability comes from studies and is presented in Table 1 of midterm report.
    ;; we generate a random number range from 1 to 100. This number falls in to one of the four ranges below, and we
    ;; assign turtles' risk factors based on this.
    let rd random 100
    ;; risk-factor = 1
    if rd >= 0 and rd < 16 [
      set risk-factor 1
    ]
    ;; risk-factor = 2
    if rd >= 16 and rd < 50 [
      set risk-factor 2
    ]
    ;; risk-factor = 3
    if rd >= 50 and rd < 80 [
      set risk-factor 3
    ]
    ;; risk-factor = 4
    if rd >= 80 and rd < 100 [
      set risk-factor 4
    ]

    set emotion_level 0  ; initialize emotion-level to be 0
    move-to one-of side-walks with [ not any? turtles-on self ]   ; move pedestrian onto the empty side walks
    set money random max-money              ;initialize the money that a pedestrian has
    set family random max-family            ;initialize the family time that a pedestrian has
    set education random max-education      ;initialize the education level that a pedestrian has
    set materialism random max-materialism  ;initialize the materialism that a pedestrian has
    set list-foes []                        ;initialize the foe list
    set from one-of goal-candidates         ; choose at random a location to start
    set goto destination-choice-people from ; choose the destination according to its decision making
    choose-best-direction-people           ; set up the best direction that pedestrians are facing
    set jaywalk false                      ; pedestrian is not jaywalking initially
    set time-to-dest-people 0              ; initialize the time to reach the destination to be 0
    set time-to-dest-incr-people 0         ; initialize the time spent on the way to the destination to be 0
  ]

  ;; create AVs
  create-AVs num-AVs
  [
    set color blue
    setup-cars                          ; use setup-cars to set up properties that are common to vehicles
    set from one-of goal-candidates     ; choose at random a location for pickup
    set goto destination-choice-AV from ; choose at location for drop-offs according to its decision making
    set passenger? true                 ; in the initial set up all AVs will start off with a passenger
    set jaywalk false                   ; AV is not jaywalking initially
    set time-to-dest-AV 0               ; initialize the time to reach the destination to be 0
    set time-to-dest-incr-AV 0          ; initialize the time spent on the way to the destination to be 0
  ]

  ;; create HVs
  create-HVs num-HVs
  [
    set color yellow
    setup-cars          ; use setup-cars to set up properties that are common to vehicles

    ;; assigns the risk-factor to hv according to probability distribution
    ;; the methodology is the same as the in People's setup
    let rd random 100
    ;; risk-factor = 1
    if rd >= 0 and rd < 16 [
      set risk-factor 1
    ]
    ;; risk-factor = 2
    if rd >= 16 and rd < 50 [
      set risk-factor 2
    ]
    ;; risk-factor = 3
    if rd >= 50 and rd < 80 [
      set risk-factor 3
    ]
    ;; risk-factor = 4
    if rd >= 80 and rd < 100 [
      set risk-factor 4
    ]
    set emotion_level 0                     ; set emotion level to be 0
    set list-foes []                        ; initialize the foe list
    set from one-of goal-candidates         ; choose at random a location to depart
    set goto destination-choice-HV from     ; choose the destination according to its decision making
    set jaywalk false                       ; HV is not jaywalking initially
    set time-to-dest-HV 0                   ; initialize the time to reach the destination to be 0
    set time-to-dest-incr-HV 0              ; initialize the time spent on the way to the destination to be 0
  ]

  reset-ticks

end

;; sets up the number of parking spots based on other parameters
to setup-parking-spots

  ask patches [

    set num-parking-spots parking-ratio * 0.01 * num-HVs   ; set up parking ratio

    ;; set up parking spots for each locations
    if num-HVs > 0 [
      let num-parking-work 1 + num-parking-spots / 4
      let num-parking-house 1 + num-parking-spots / 4
      let num-parking-school 1 + num-parking-spots / 4
      let num-parking-shop 1 + num-parking-spots / 4
      set num-parking-spots num-parking-work + num-parking-house + num-parking-school + num-parking-shop  ; uodate the total number of parking spots

    ;; set the spots next to the intersections to be green to avoid double parking issues
    if (abs pxcor = 1 and abs pycor = 1) or (abs pxcor = 1 and abs pycor = 2) or (abs pxcor = 2 and abs pycor = 1)
    [set pcolor grey + 1]

    if count patches with [pcolor = orange and pxcor > 2]  < (num-parking-work) [
      if (any? neighbors with [pcolor = grey]) and pcolor != grey and pcolor != red and pcolor != green and pcolor != grey + 1 and pxcor > 2
      [set pcolor orange
       set occupied? false]
    ]

    if count patches with [pcolor = orange and pxcor < -2] < (num-parking-house) [
      if (any? neighbors with [pcolor = grey]) and pcolor != grey and pcolor != red and pcolor != green and pcolor != grey + 1 and pxcor < -2
      [set pcolor orange
       set occupied? false]
    ]

    if count patches with [pcolor = orange and pycor > 2] < (num-parking-shop) [
      if (any? neighbors with [pcolor = grey]) and pcolor != grey and pcolor != red and pcolor != green and pcolor != grey + 1 and pycor > 2
      [set pcolor orange
       set occupied? false]
    ]

    if count patches with [pcolor = orange and pycor < -2] < (num-parking-school) [
      if (any? neighbors with [pcolor = grey]) and pcolor != grey and pcolor != red and pcolor != green and pcolor != grey + 1 and pycor < -2
      [set pcolor orange
       set occupied? false]
    ]

    ]

    ;; This sets up the parking spots for each location
    if (abs pxcor = 1 and pycor > 0 and pcolor = orange)
    [set shop? true]
    if (pxcor < 0 and abs pycor = 1 and pcolor = orange)
    [set house? true]
    if (pxcor > 0 and abs pycor = 1 and pcolor = orange)
    [set work? true]
    if (abs pxcor = 1 and pycor < 0 and pcolor = orange)
    [set school? true]
  ]

  ;; creates lists for parking spots of each location
  set list-parking-house patches with [house? = true]
  set list-parking-shop patches with [shop? = true]
  set list-parking-school patches with [school? = true]
  set list-parking-work patches with [work? = true]
  set list-parking-spot patches with [pcolor = orange]
end

;; set up our destinations (draw + set goal patches)
to setup-places
  ;; for each locations, we paint its color and display its name
  ask patches [
  if (pxcor = 10 and pycor = 3)
   [set pcolor black
    set plabel "work"]
  if (pxcor = -10 and pycor = -3)
    [set pcolor yellow
    set plabel "house"]
  if (pxcor = 3 and pycor = -10)
    [set pcolor 86
    set plabel "school"]
  if (pxcor = -3 and pycor = 10)
    [set pcolor 42
    set plabel "shop"]
  ]
  ;; we are setting our goal patches for drop-offs and pick-up (AVs) / look for parking (HVs)
  set work patches with [pxcor = 10 and pycor = 0]
  set house patches with [pxcor = -10 and pycor = 0]
  set school patches with [pxcor = 0 and pycor = -10]
  set shop patches with [pxcor = 0 and pycor = 10]
end

;; Initialize the global variables to appropriate values
to setup-globals

  ;; set up max values of destinations
  set max-money 40
  set max-family 40
  set max-education 40
  set max-materialism 40

  ;; initialize some variables to 0 since it is the beginning of simulation
  set phase 0
  set num-goals-met 0

  ;; set up amount of HVs that one AV can replace to be 10, based on the studies (explained in previous report)
  set replacement-rate 10
  ;; set up total number of cars, number of AVs, number of HVs, according to some user's input
  set total-num-cars floor(num-car-users / ((replacement-rate * percentage-of-AVs / 100) + 1))
  set num-AVs floor(total-num-cars * percentage-of-AVs / 100)
  set num-AV-people num-AVs * replacement-rate
  set num-HVs total-num-cars - num-AVs
  set num-HV-people num-HVs

  ;; number of AV people that exceeds number of AVs - these people will need to start out at locations
  let extra-AV-people num-AV-people - num-AVs

  ;; randomly disperses the extra AV passengers amongst our different locations
  set AV-people-count-house random extra-AV-people
  set AV-people-count-work random (extra-AV-people - AV-people-count-house)
  set AV-people-count-school random (extra-AV-people - ( AV-people-count-house + AV-people-count-work))
  set AV-people-count-shop extra-AV-people - ( AV-people-count-house + AV-people-count-work + AV-people-count-school)

  ;; don't make acceleration 0.1 since we could get a rounding error and end up on a patch boundary
  set acceleration 0.099
end

;; Make the patches have appropriate colors, set up the roads and intersections agentsets,
;; and initialize the traffic lights to one setting
to setup-patches
  ;; initialize the patch-owned variables and color the patches to a base-color
  ask patches [
    set intersection? false
    set auto? false
    set green-light-up? true
    set my-phase -1
    set pcolor green - 0.5 - random-float 0.5

    if (pxcor = -3 and pycor = 0)
    [set light-here? true]
    if (pxcor = 0 and pycor = -3)
    [set light-here? true]
  ]

  setup-places  ; set up and draw 4 places (house, school, shop, work)

  ;; set up new roads, intersections, cross-walks
  set roads patches with [
    abs pxcor = 0 or abs pycor = 0
  ]
  set intersections roads with [
    pxcor = 0 and pycor = 0
  ]
  set cross-walks patches with [
    ;(abs pxcor <= 3) and (abs pycor <= 3) and pcolor != white
    (pxcor = -3 and pycor = 0) or (pxcor = -1 and pycor = 0) or (pxcor = 0 and pycor = -1) or (pxcor = 0 and pycor = -3)
  ]
  set p-cross-walks patches with [
    (pxcor = -1 and pycor = -2) or (pxcor = -2 and pycor = -1)
  ]

  ;; sets up roads for each location to better predict congestion
  set work-road patches with [
    pxcor > 0 and pycor = 0
  ]
  set house-road patches with [
    pxcor < 0 and pycor = 0
  ]
  set shop-road patches with [
    pxcor = 0 and pycor > 0
  ]
  set school-road patches with [
    pxcor = 0 and pycor < 0
  ]

  ;; set up roads that parking spots will be on
  set parking-roads patches with [
    (abs pycor = 1 and abs pxcor > 0) or (abs pxcor = 1 and abs pycor > 0)
  ]

  ;; set up side walk for pedestrians to walk on
  set side-walks patches with [
    (pycor = -2 and abs pxcor > 1) or (pxcor = -2 and abs pycor > 1)
  ]

  ask roads [ set pcolor grey ]
  ask parking-roads [ set pcolor grey - 3]
  ask side-walks [ set pcolor white]
  setup-intersections
  setup-parking-spots
end

;; Give the intersections appropriate values
;; Make all the traffic lights start off so that the lights are red
;; horizontally and green vertically.
to setup-intersections
  ask intersections [
    set intersection? true
    set green-light-up? true
    set my-phase 0
    set auto? true
    set-signal-colors
  ]
end

;; Initialize the turtle variables to appropriate values and place the turtle on an empty road patch.
to setup-cars  ;; turtle procedure
  set shape  "car"
  set size 1.1
  set speed 0
  set wait-time 0
  set on? true

  ;; initialize money, family, education and materialism to random value for each turtles
  set money random max-money
  set family random max-family
  set education random max-education
  set materialism random max-materialism

  put-on-empty-road               ;; places cars on empty spot on road
  ask HVs [choose-best-direction] ;; place HVs on road so they are facing direction of their best destination
  ask AVs [choose-best-direction] ;; place AVs on road so they are facing direction of their best destination

end


;; Find a road patch without any turtles on it and place the turtle there.
to put-on-empty-road  ;; turtle procedure
  let temp one-of roads with [ not any? turtles-on self]
  while [ member? temp cross-walks]
  [set temp one-of roads with [ not any? turtles-on self]]
  move-to temp
end


;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; Run the simulation
to go

  ;; sets up traffic lights
  set-signals

  ;; specifc tasks for HVs
  ask HVs [
  ;; decay emotions
  decay-emotion

  ;; check whether HV reaches its destination
  ;; if it is not at the destination,
  ;; it should choose its best direction to go based on its destination
  ;; and it should drive toward its destination
  ifelse reachDestination = false
  [
      set on? true
      choose-best-direction
      drive-HV
      set time-to-dest-incr-HV time-to-dest-incr-HV + 1
      count-wait-time
  ]
  ;; if it is at the destination,
  ;; we should check whether it parks in the parking spot
    [
    ; check if the car is parked in lot or not
     ;if it is parked
     ifelse on? = false
    [
      ;if the parking time is up
      ifelse count-down <= 0
      [ move-back-to-road ]
      ;if parking time not up
      [decrement-counter]

    ]
    ; the car is not parked
    [
       ;; create temporary variable for current location and look for parking spot next to current location
      let cur-spot find-spot
      let x [pxcor] of cur-spot
      let y [pycor] of cur-spot

        ;; first, a hv looks for parking spots, and report whether it found one or not
      ifelse x != 0 and y != 0
      ;the hv found a spot
      [
        move-to-spot x y ; hv will move to open parking spot
        set time-to-dest-HV time-to-dest-incr-HV
        set time-to-dest-incr-HV 0
        set-counter ;; sets parking count down
      ]
      ;the car does not have a parking spot
      [

        go-around ;; keep driving around until spot opens up
      ]
    ]
    ]
  ]

  ;; specific tasks for AVs
  ask AVs [
    ; if reached an AV destination
    ifelse destination-AVs?
    [
      if passenger?
      ;if there is passenger, drop them off
      [
        drop-off-passengers
        set time-to-dest-AV time-to-dest-incr-AV
        set time-to-dest-incr-AV 0
      ]
      ;pick up passengers
      pick-up-passengers

      ;set new destination for AV
      set goto destination-choice-AV goto ;; update to best AV choice
    ]
    ;if this is not destination, keep driving
    [
    set on? true
    set time-to-dest-incr-AV time-to-dest-incr-AV + 1
    choose-best-direction
    drive-AV
    count-wait-time
    ]
  ]

  ask People [
;; decay emotions
  decay-emotion
  ;; check whether pedestrian reaches its destination
  ;; if it is not at the destination,
  ;; it should choose its best direction to go based on its destination
  ;; and it should walk toward its destination
  ifelse reachDestination-people = false
  ;if this is not destination
  [
      set on? true
      choose-best-direction-people
      walk
      set time-to-dest-incr-people time-to-dest-incr-people + 1
      count-wait-time
  ]
 ;if this is destination
    [
    ; check if the person is in destination or not
     ;if it is in destination
     ifelse on? = false
    [
      ;if the time in location is up
      ifelse count-down <= 0
      [ walk-back-to-road ]
      ;if time in location is not up
      [decrement-counter]

    ]
    ; the person is not inside
    [
        move-to-spot-people ; people will move to the destinations
        set time-to-dest-people time-to-dest-incr-people
        set time-to-dest-incr-people 0
        set-counter-pedestrian ;; sets count down
    ]
    ]
  ]
   ; check for collisions
   ask patches [
   check-for-collisions
   ]


  next-phase ;; update the phase and the global clock
  tick
end

;; function to check if there has been a collision, then kills off the agents involved
to check-for-collisions

    ifelse (pxcor = -9 and pycor = -3) or (pxcor = 11 and pycor = 3) or (pxcor = 3 and pycor = -11) or (pxcor = -3 and pycor = 11)
    [
      ; do nothing
    ]
    [
     if count turtles-here > 1 [
     ask turtles-here [ die ]
     ]
    ]
end

;; chooses the direction for HV, AV
to choose-best-direction
 ;; if on horizontal road
 if (pycor = 0 and pxcor != 0) [
   set heading 90
   set up-car? false
  ]
  ;; if on vertical road
  if (pxcor = 0 and pycor != 0) [
  set heading 0
  set up-car? true
  ]
  ;; if at intersection choose which way to go based on its destination
  if (pycor = 0 and pxcor = 0) [
   if (goto = one-of work or goto = one-of house) [
    set heading 90
    set up-car? false
    ]
   if (goto = one-of shop or goto = one-of school) [
    set heading 0
    set up-car? true
    ]
  ]
end

;; chooses the direction for pedestrian
to choose-best-direction-people
 ;; if on horizontal road
 if (pycor = -2 and pxcor != -2) [
   set heading 90
  ]
  ;; if on vertical road
  if (pxcor = -2 and pycor != -2) [
  set heading 0
  ]
  ;; if at intersection choose which way to go based on highest utility destination
  if (pycor = -2 and pxcor = -2) [
   if (goto = one-of work or goto = one-of house) [
    set heading 90
    set up-car? false
    ]
   if (goto = one-of shop or goto = one-of school) [
    set heading 0
    set up-car? true
    ]
  ]
end

to drive-HV

      ;; checks to see if a turtle has run a red light
    if [pcolor = red] of patch-here [set jaywalk true]
    ;; checks to see if turtle has past the full intersection and sets jaywalk back to false
    if [abs pxcor > 2 or abs pycor > 2] of patch-here [set jaywalk false]

    ;; if a turtle is near the intersection and there are other turtles breaking the law, emotions will go up
    if [abs pxcor <= 3 and abs pycor <= 3] of patch-here
    [set emotion_level emotion_level + (2 * count People with [jaywalk]) ;; more remorsed by people jaywalking
     set emotion_level emotion_level + (count HVs with [jaywalk]) ;; less remorsed by HVs running red light

  ]

  ifelse member? patch-here cross-walks [
     ; if turtle is in the cross-walk area - use utility function to determine if it should go or stop
      ;; check # of each turtle in cross-walk, check if red light ahead, check the level of value need
      ;; if u(x) > risk threshold -> stop, if u(x) < risk threshold -> go
      let red-light-ahead 0
      ifelse [pcolor = red] of patch-ahead 1
      [set red-light-ahead 1]
      [set red-light-ahead 0]

    ;; decide on trade-offs: compare values between the utility of keeping safe and utility of going to destination
      ifelse risk-level_HV red-light-ahead > destination-utility-HV
      [fd 0 set speed 0]
      [fd 1 set speed 1]
  ]
   ;; if a turtle is not at intersection, it goes or stop depends on if there are turtle ahead
  [
    ifelse any? turtles-on patch-ahead 1
    [fd 0 set speed 0]
    [fd 1 set speed 1]
  ]

end

to drive-AV
  ifelse [pcolor = red] of patch-ahead 1 [
    fd 0
    set speed 0
  ]
  [
  ifelse any? turtles-on patch-ahead 1
    [fd 0 set speed 0]
    [fd 1 set speed 1]
  ]
end


to walk ;; for pedestrians
  ;; checks to see if a turtle is in intersection when light is green
    if [pcolor = green] of patch-here [set jaywalk true]
        ;; checks to see if turtle has past the full intersection and sets jaywalk back to false
    if [abs pxcor > 2 or abs pycor > 2] of patch-here [set jaywalk false]
    ;; if a turtle is near the intersection and there are other turtles breaking the law, emotions will go up
    if [abs pxcor <= 3 and abs pycor <= 3] of patch-here
    [set emotion_level emotion_level + (count HVs with [abs pxcor <= 2 and abs pycor <= 2])] ;; remorsed by HVs running red light

  ifelse member? patch-here p-cross-walks [
      ; if turtle is in the cross-walk area - use utility function to determine if it should go or stop
      ;; check # of each turtle in cross-walk, check if red light ahead, check the level of value need
      ;; if u(x) > risk threshold -> stop, if u(x) < risk threshold -> go
      let red-light-ahead 0
    if [pycor] of patch-here = -2 [
      if [pcolor = red] of patch-at -2 0 [set red-light-ahead 1]
    ]

    if [pycor] of patch-here = -1 [
      if [pcolor = red] of patch-at 0 -2 [set red-light-ahead 1]
    ]
      ;; decide on trade-offs: compare values between the utility of keeping safe and utility of going to destination
      ifelse risk-level_ped (red-light-ahead) > destination-utility-people
      [fd 0 set speed 0]
      [fd 1 set speed 1]
  ]
  ;; if a turtle is not at intersection, it goes or stop depends on if there are turtle ahead
  [
    ifelse any? turtles-on patch-ahead 1
    [fd 0 set speed 0]
    [fd 1 set speed 1]
  ]
end

;; Perceived risk for HV
to-report risk-level_HV [red-light-ahead]
  let countHVs count HVs-on patch-right-and-ahead 1 1 + count HVs-on patch-left-and-ahead 1 1 + count HVs-on patch-ahead 1
  let countAVs count AVs-on patch-right-and-ahead 1 1 + count AVs-on patch-left-and-ahead 1 1 + count AVs-on patch-ahead 1
  let countPeople count People-on patch-right-and-ahead 1 1 + count People-on patch-left-and-ahead 1 1 + count People-on patch-ahead 1

  ;; checks if agents ahead are foes
  let foe-ahead 0

  ; case: check if turtle on patch right and ahead is jaywalking - add to foe list if true
  if count turtles-on patch-right-and-ahead 1 1 > 0 [
    if check-foe [who] of turtles-on patch-right-and-ahead 1 1 = 1 [set foe-ahead foe-ahead + 1]
    if [jaywalk] of turtles-on patch-right-and-ahead 1 1 = true
    [if check-foe [who] of turtles-on patch-right-and-ahead 1 1 = 0
      [set list-foes lput [who] of turtles-on patch-right-and-ahead 1 1 list-foes]
    ]
  ]

    ; case: check if turtle on patch left and ahead is jaywalking - add to foe list if true
  if count turtles-on patch-left-and-ahead 1 1 > 0 [
    if check-foe [who] of turtles-on patch-left-and-ahead 1 1 = 1 [set foe-ahead foe-ahead + 1]
    if [jaywalk] of turtles-on patch-left-and-ahead 1 1 = true
    [if check-foe [who] of turtles-on patch-left-and-ahead 1 1 = 0
      [set list-foes lput [who] of turtles-on patch-left-and-ahead 1 1 list-foes]
    ]
  ]

    ; case: check if turtle on patch ahead is jaywalking - add to foe list if true
  if count turtles-on patch-ahead 1 > 0 [
    if check-foe [who] of turtles-on patch-ahead 1 = 1 [set foe-ahead foe-ahead + 1]
    if [jaywalk] of turtles-on patch-ahead 1 = true
    [if check-foe [who] of turtles-on patch-ahead 1 = 0
      [set list-foes lput [who] of turtles-on patch-ahead 1 list-foes]
    ]
  ]
  ; we use the different weights from our MODM trees in order to come up with this utility function
  ; this utility function is the perceived risk and takes factors such as an agent risk level,
  ; the number of cars ahead, the number of pedestrian ahead, the traffic light, an agent's emotion level,
  ; and whether the agent ahead is in the current agent's foe list
  ; this perceived risk will be compared with the destination utility in order to decide whether to stop or go.
  let u_risk (5 - risk-factor) * (0.12 * (countHVs + countAVs) + 0.46 * (countPeople)
    + 0.06 * red-light-ahead) - 0.03 * emotion_level - 0.03 * foe-ahead
  report u_risk
end

;; Perceived risk for pedestrian
to-report risk-level_ped [red-light-ahead]
  let countHVs count HVs-on patch-right-and-ahead 1 1 + count HVs-on patch-left-and-ahead 1 1 + count HVs-on patch-ahead 1
  let countAVs count AVs-on patch-right-and-ahead 1 1 + count AVs-on patch-left-and-ahead 1 1 + count AVs-on patch-ahead 1
  let countPeople count People-on patch-right-and-ahead 1 1 + count People-on patch-left-and-ahead 1 1 + count People-on patch-ahead 1

  let foe-ahead 0
  ; case: check if turtle on patch right and ahead is jaywalking - add to foe list if true
  if count turtles-on patch-right-and-ahead 1 1 > 0 [
    if check-foe [who] of turtles-on patch-right-and-ahead 1 1 = 1 [set foe-ahead foe-ahead + 1]
    if [jaywalk] of turtles-on patch-right-and-ahead 1 1 = true
    [if check-foe [who] of turtles-on patch-right-and-ahead 1 1 = 0
      [set list-foes lput [who] of turtles-on patch-right-and-ahead 1 1 list-foes]
    ]
  ]

    ; case: check if turtle on patch left and ahead is jaywalking - add to foe list if true
  if count turtles-on patch-left-and-ahead 1 1 > 0 [
    if check-foe [who] of turtles-on patch-left-and-ahead 1 1 = 1 [set foe-ahead foe-ahead + 1]
    if [jaywalk] of turtles-on patch-left-and-ahead 1 1 = true
    [if check-foe [who] of turtles-on patch-left-and-ahead 1 1 = 0
      [set list-foes lput [who] of turtles-on patch-left-and-ahead 1 1 list-foes]
    ]
  ]

  ; case: check if turtle on patch ahead is jaywalking - add to foe list if true
  if count turtles-on patch-ahead 1 > 0 [
    if check-foe [who] of turtles-on patch-ahead 1 = 1 [set foe-ahead foe-ahead + 1]
    if [jaywalk] of turtles-on patch-ahead 1 = true
    [if check-foe [who] of turtles-on patch-ahead 1 = 0
      [set list-foes lput [who] of turtles-on patch-ahead 1 list-foes]
    ]
  ]
  ; we use the different weights from our MODM trees in order to come up with this utility function
  ; this utility function is the perceived risk and takes factors such as an agent risk level,
  ; the number of cars ahead, the traffic light, an agent's emotion level,
  ; and whether the agent ahead is in the current agent's foe list
  ; this perceived risk will be compared with the destination utility in order to decide whether to stop or go.
  let u_risk (5 - risk-factor) * (0.23 * (countHVs + countAVs)
    + 0.03 * red-light-ahead) - 0.09 * emotion_level - 0.03 * foe-ahead
  report u_risk
end

;; have the traffic lights change color if phase equals each intersections' my-phase
to set-signals
  ask roads with [ auto? and phase = floor ((my-phase * ticks-per-cycle) / 100) ] [
    set green-light-up? (not green-light-up?)
    set-signal-colors
  ]
end

;; This procedure checks the variable green-light-up? at each intersection and sets the
;; traffic lights to have the green light up or the green light to the left.
to set-signal-colors  ;; intersection (patch) procedure
    ifelse green-light-up? [
      ask patch-at -2 0 [ set pcolor red ]
      ask patch-at 0 -2 [ set pcolor green ]
    ]
    [
      ask patch-at -2 0 [ set pcolor green ]
      ask patch-at 0 -2 [ set pcolor red ]
    ]
end

;; keep track of the amount of time a car has been stopped
;; if its speed is 0 and it is not parked, we increment its wait time
;; if the wait-time is greater than 10, we increment turtle's emotion level
to count-wait-time  ;; turtle procedure
  ifelse speed = 0 and on? = true [
    set wait-time wait-time + 1
  ]
  [ set wait-time 0 ]

  if wait-time > 10 [
    set emotion_level emotion_level + 2
  ]
end

;; cycles phase to the next appropriate value
to next-phase
  ;; The phase cycles from 0 to ticks-per-cycle, then starts over.
  set phase phase + 1
  if phase mod ticks-per-cycle = 0 [ set phase 0 ]
end

;; method to check if AV has reached its destination
to-report destination-AVs?
  let reach? false

  ; four cases for house, work, school, and shop
  if goto = one-of house [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x = -10 and y = 0 [
      set reach? true
    ]
  ]
  if goto = one-of work [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x = 10 and y = 0 [
      set reach? true
    ]
  ]
  if goto = one-of school [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x = 0 and y = -10 [
      set reach? true
    ]
  ]
  if goto = one-of shop [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x = 0 and y = 10 [
      set reach? true
    ]
  ]
  report reach?
end

;; method to drop off AV passengers if reached destination
to drop-off-passengers

  if goto = one-of house [
    set AV-people-count-house AV-people-count-house + 1
    ;print "dropped off from house"
    set passenger? false
    set num-goals-met num-goals-met + 1
  ]
  if goto = one-of work [
    set AV-people-count-work AV-people-count-work + 1
    ;print "dropped off from work"
    set passenger? false
    set num-goals-met num-goals-met + 1
  ]
  if goto = one-of school [
    set AV-people-count-school AV-people-count-school + 1
    ;print "dropped off from school"
    set passenger? false
    set num-goals-met num-goals-met + 1
  ]
  if goto = one-of shop [
    set AV-people-count-shop AV-people-count-shop + 1
    ;print "dropped off from shop"
    set passenger? false
    set num-goals-met num-goals-met + 1
  ]
end

;; method to pick-up new AV passenger
to pick-up-passengers
  if goto = one-of house [
    ; checks if there is an AV passenger at current location
    if AV-people-count-house > 1 [
      set AV-people-count-house AV-people-count-house - 1
      ;print "picked up from house"
      set passenger? true
      ;print passenger?
    ]
  ]
  if goto = one-of work [
    if AV-people-count-work > 1
    [
      set AV-people-count-work AV-people-count-work - 1
      ;print "picked up from work"
      set passenger? true
      ;print passenger?
    ]
  ]
  if goto = one-of school [
    if AV-people-count-school > 1
    [
      set AV-people-count-school AV-people-count-school - 1
      ;print "picked up from school"
      set passenger? true
      ;print passenger?
    ]
  ]
  if goto = one-of shop [
    if AV-people-count-shop > 1
    [
      set AV-people-count-shop AV-people-count-shop - 1
      ;print "picked up from shop"
      set passenger? true
      ;print passenger?
    ]
  ]
end

;; method to see if HV has reached destination
to-report reachDestination
  let reach? false
  ; 4 cases for each destination
  ; case 1
  if goto = one-of house [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    ;; checks for range of road where an HV can park for their destination
    if x >= -18 and x <= -1 [
      set reach? true
    ]
  ]
  ;case 2
  if goto = one-of work [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x <= 18 and x >= 1 [
      set reach? true
    ]
  ]
  ;case 3
  if goto = one-of school [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if y >= -18 and y <= -1 [
      set reach? true

    ]
  ]
  ;case 4
  if goto = one-of shop [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if y >= 1 and y <= 18 [
      set reach? true
    ]
  ]
  report reach?
end


;; method to see if people has reached destination
to-report reachDestination-people
  let reach? false
  ; 4 cases for each destination
  ; case 1
  if goto = one-of house [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x >= -18 and x <= 10 and y = -2 [
      set reach? true
    ]
    if x = -9 and y = -3 [
      set reach? true
    ]
  ]
  ;case 2
  if goto = one-of work [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if x <= 18 and x >= 10 and y = -2 [
      set reach? true
    ]
    if x = 11 and y = 3 [
      set reach? true
    ]
  ]
  ;case 3
  if goto = one-of school [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if y >= -18 and y <= -10 and x = -2 [
      set reach? true
    ]
    if x = 3 and y = -11 [
      set reach? true
    ]
  ]
  ;case 4
  if goto = one-of shop [
    let x [pxcor] of patch-here
    let y [pycor] of patch-here
    if y >= 10 and y <= 18 and x = -2 [
      set reach? true
    ]
    if x = -3 and y = 11 [
      set reach? true
    ]
  ]
  report reach?
end

;; if hv has found an open spot, move to that spot to park
to move-to-spot [x y]
    setxy x y
    set on? false ;; turns off car while parked
    set speed -1

  ;; changes the patch of the parking spot to be occupied
    ask patches with [pxcor = x and pycor = y] [
      set occupied? true
    ]
  ; updates number of goals met
    set num-goals-met num-goals-met + 1

end

;; move pedestrians to inside their destination
to move-to-spot-people
    if goto = one-of house
    [
      setxy -9 -3
      set on? false
    ]
    if goto = one-of work
    [
      setxy 11 3
      set on? false
    ]
    if goto = one-of school
    [
      setxy 3 -11
      set on? false
    ]
    if goto = one-of shop
    [
      setxy -3 11
      set on? false
    ]

  ; updates number of goals met
    set num-goals-met num-goals-met + 1

end



;; reports if a parking spot has been found
to-report find-spot
  let found false
  let spot one-of patches with [pxcor = 0 and pycor = 0]

    ; case 1: checks the two patches between the road for house parking spots
    if goto = one-of house [
      let spot1 one-of neighbors4 with [ pycor > 0 ]
      let spot2 one-of neighbors4 with [ pycor < 0 ]
    ;; checks for parking spot of first neighbor
        if member? spot1 list-parking-house [
          set spot spot1
          ;; checks if parking spot is occupied, if not occupied sets found spot to true
          set found not [occupied?] of spot1
          ;; if does not find a parking spot, if other HVs occupy this parking spot,
          ;; we add other HV to this HV's foe list
          if not found [
          let foeHV [who] of HVs with [pxcor = [pxcor] of spot1 and pycor = [pycor] of spot1]
          if check-foe foeHV = 0 [set list-foes lput foeHV list-foes]
        ]
    ]

    ;; checks for parking spot of second neighbor, same logic as above
        if member? spot2 list-parking-house and not found [
          set spot spot2
          set found not [occupied?] of spot2
          if not found [
          let foeHV [who] of HVs with [pxcor = [pxcor] of spot2 and pycor = [pycor] of spot2]
          if check-foe foeHV = 0 [set list-foes lput foeHV list-foes]]
        ]
      ]
   ;case 2: checks for open parking spots for work parking, same logic as case 1
   if goto = one-of work [
      let spot1 one-of neighbors4 with [ pycor > 0 ]
      let spot2 one-of neighbors4 with [ pycor < 0 ]

      if member? spot1 list-parking-work [
          set spot spot1
          set found not [occupied?] of spot1
          if not found [
          let foeHV [who] of HVs with [pxcor = [pxcor] of spot1 and pycor = [pycor] of spot1]
          if check-foe foeHV = 0 [set list-foes lput foeHV list-foes]]
        ]
      if member? spot2 list-parking-work and not found [
          set spot spot2
          set found not [occupied?] of spot2
          if not found [
          let foeHV [who] of HVs with [pxcor = [pxcor] of spot2 and pycor = [pycor] of spot2]
          if check-foe foeHV = 0 [set list-foes lput foeHV list-foes]]
        ]
    ]
  ; case 3: checks for open parking spots for shop parking, same logic as case 1
   if goto = one-of shop [
      let spot1 one-of neighbors4 with [ pxcor > 0 ]
      let spot2 one-of neighbors4 with [ pxcor < 0 ]

      if member? spot1 list-parking-shop [
          set spot spot1
          set found not [occupied?] of spot1
          if not found [
          let foeHV [who] of HVs with [pxcor = [pxcor] of spot1 and pycor = [pycor] of spot1]
          if check-foe foeHV = 0 [set list-foes lput foeHV list-foes]]
        ]
      if member? spot2 list-parking-shop and not found [
          set spot spot2
          set found not [occupied?] of spot2
          if not found [
          let foeHV [who] of HVs with [pxcor = [pxcor] of spot2 and pycor = [pycor] of spot2]
          if check-foe foeHV = 0 [set list-foes lput foeHV list-foes]
          ]
        ]
    ]
  ; case 4: checks for open parking spots for school parking, same logic as case 1
   if goto = one-of school [
      let spot1 one-of neighbors4 with [ pxcor > 0 ]
      let spot2 one-of neighbors4 with [ pxcor < 0 ]

      if member? spot1 list-parking-school [
          set spot spot1
          set found not [occupied?] of spot1
          if not found [
          let foeHV [who] of HVs with [pxcor = [pxcor] of spot1 and pycor = [pycor] of spot1]
          if check-foe foeHV = 0 [set list-foes lput foeHV list-foes]
        ]
    ]

      if member? spot2 list-parking-school and not found [
          set spot spot2
          set found not [occupied?] of spot2
          if not found [
          let foeHV [who] of HVs with [pxcor = [pxcor] of spot2 and pycor = [pycor] of spot2]
          if check-foe foeHV = 0 [set list-foes lput foeHV list-foes]
        ]
    ]
  ]


; if found a parking spot that is not occupied, it will report that spot
; if occupied it will return x = 0 and y = 0
  ifelse found [report spot]
  [report one-of patches with [pxcor = 0 and pycor = 0]] ;; save the hv in this spot to make a foe!
end

;; function to check if an agent is a member of another agent's foe list
to-report check-foe [id]
  let count-id 0
  ; iterate through agent's foe list, increment count if we see the element = id
  foreach list-foes [
    [a] -> if id = a [set count-id count-id + 1]
  ]
  ifelse count-id > 0
  [report 1] ; if agent is a foe
  [report 0] ; if agent is not a foe

end

;; if the HV didn't find a spot, keep driving until find one (may modify this to make a decision)
to go-around
   ; update emotion level since cars will experience negative emotions when they cannot find parking
   set emotion_level emotion_level + 2
   drive-HV
end

;; utility function for HVs to choose their goal destination
to-report destination-choice-HV [previous-destination]
  check-d ;; function that sets the distances from each location
  ; we use the weights from MODM trees, and for utility, we want to maximizing the need and minimizing the distance
  let u_greatest 0
  let u_work (0.21 * (max-money - money)) / (d_work)
  let u_shop (0.02 * (max-materialism - materialism)) / (d_shop)
  let u_school (0.05 * (max-education - education)) / (d_school)
  let u_house (0.08 * (max-family - family)) / (d_house)

  ;; checks for which location has greatest utility and then reports that location
  if previous-destination = one-of house [
    set u_greatest max (list u_work u_shop u_school)
  ]
  if previous-destination = one-of work [
    set u_greatest max (list u_house u_shop u_school)
  ]
  if previous-destination = one-of shop [
    set u_greatest max (list u_work u_house u_school)
  ]
  if previous-destination = one-of school [
    set u_greatest max (list u_work u_shop u_house)
  ]
  if u_greatest = u_work [report one-of work]
  if u_greatest = u_shop [report one-of shop]
  if u_greatest = u_school [report one-of school]
  if u_greatest = u_house [report one-of house]
end


;; utility function for people to choose their goal destination
to-report destination-choice-people [previous-destination]
  check-d-person ;; function that sets the distances from each location
  ; we use the weights from MODM trees, and for utility, we want to maximizing the need and minimizing the distance
  let u_greatest 0
  let u_work (0.4 * (max-money - money)) / (d_work)
  let u_shop (0.04 * (max-materialism - materialism)) / (d_shop)
  let u_school (0.12 * (max-education - education)) / (d_school)
  let u_house (0.18 * (max-family - family)) / (d_house)

  ;; checks for which location has greatest utility and then reports that location
  if previous-destination = one-of house [
    set u_greatest max (list u_work u_shop u_school)
  ]
  if previous-destination = one-of work [
    set u_greatest max (list u_house u_shop u_school)
  ]
  if previous-destination = one-of shop [
    set u_greatest max (list u_work u_house u_school)
  ]
  if previous-destination = one-of school [
    set u_greatest max (list u_work u_shop u_house)
  ]
  if u_greatest = u_work [report one-of work]
  if u_greatest = u_shop [report one-of shop]
  if u_greatest = u_school [report one-of school]
  if u_greatest = u_house [report one-of house]
end

;; utility function for AVs to choose their goal destination
to-report destination-choice-AV [previous-destination]
  check-d ;; function that sets the distances from each location
  ;; finds the number of cars on each road to check for congestion
  let congestion_work count turtles-on work-road
  let congestion_school count turtles-on school-road
  let congestion_house count turtles-on house-road
  let congestion_shop count turtles-on shop-road
  ; we use the weights from MODM trees, and for utility, we want to minimizing the distance and the time it takes to destination
  let u_greatest 0
  let u_work 1 / (d_work + congestion_work)
  let u_shop 1 / (d_shop + congestion_shop)
  let u_school 1 / (d_school + congestion_school)
  let u_house 1 / (d_house + congestion_house)

 ;; checks for which location has greatest utility and then reports that location
  if previous-destination = one-of house [
    set u_greatest max (list u_work u_shop u_school)
  ]
  if previous-destination = one-of work [
    set u_greatest max (list u_house u_shop u_school)
  ]
  if previous-destination = one-of shop [
    set u_greatest max (list u_work u_house u_school)
  ]
  if previous-destination = one-of school [
    set u_greatest max (list u_work u_shop u_house)
  ]
  if u_greatest = u_work [report one-of work]
  if u_greatest = u_shop [report one-of shop]
  if u_greatest = u_school [report one-of school]
  if u_greatest = u_house [report one-of house]
end

;; report the utility of going to the best destination
to-report destination-utility-HV
  if goto = one-of work [
    if money < high-money-need-HV [report 0.21 * 2]  ;; high need
    if money < med-money-need-HV [report 0.21 * 1]  ;; med need
    if money >= med-money-need-HV [report 0.21 * 0] ;; low need
  ]
  if goto = one-of shop [
    if materialism < high-materialism-need-HV [report 0.02 * 2]  ;; high need
    if materialism < med-materialism-need-HV [report 0.02 * 1]  ;; med need
    if materialism >= med-materialism-need-HV [report 0.02 * 0] ;; low need
  ]
  if goto = one-of school [
    if education < high-education-need-HV [report 0.05 * 2]  ;; high need
    if education < med-education-need-HV [report 0.05 * 1]  ;; med need
    if education >= med-education-need-HV [report 0.05 * 0] ;; low need
 ]
  if goto = one-of house [
    if family < high-family-need-HV [report 0.08 * 2]  ;; high need
    if family < med-family-need-HV [report 0.08 * 1]  ;; med need
    if family >= med-family-need-HV [report 0.08 * 0] ;; low need
  ]
end

;; report the utility of going to the best destination
to-report destination-utility-people
    if goto = one-of work [
    if money < high-need-money-Ped [report 0.4 * 2]  ;; high need
    if money < med-need-money-Ped [report 0.4 * 1]  ;; med need
    if money >= med-need-money-Ped [report 0.4 * 0] ;; low need
  ]
  if goto = one-of shop [
    if materialism < high-need-materialism-Ped [report 0.04 * 2]  ;; high need
    if materialism < med-need-materialism-Ped [report 0.04 * 1]  ;; med need
    if materialism >= med-need-materialism-Ped [report 0.04 * 0] ;; low need
  ]
  if goto = one-of school [
    if education < high-need-education-Ped [report 0.12 * 2]  ;; high need
    if education < med-need-education-Ped [report 0.12 * 1]  ;; med need
    if education >= med-need-education-Ped [report 0.12 * 0] ;; low need
 ]
  if goto = one-of house [
    if family < high-need-family-Ped [report 0.18 * 2]  ;; high need
    if family < med-need-family-Ped [report 0.18 * 1]  ;; med need
    if family >= med-need-family-Ped [report 0.18 * 0] ;; low need
  ]
end

;; this sets all of the different distances form a vehicle current location to the beginning of a destination road
;; it incorporates the fact that these are one way streets
to check-d
  ;; this will set distance values for each location - values can be 1-4 depending on order of how far each locations road is
    let x [pxcor] of patch-here
    let y [pycor] of patch-here

  ;; if on horizontal road
  if y = 0 [
    ifelse x < 0
    [set d_house 1]  ;; if you are currently on the house road
    [set d_house 2]  ;; if you are currently on the work road

    ifelse x < 0
    [set d_work 2]      ;; if you are currently on the house road
    [set d_work 1]      ;; if you are currently on the work road

    ifelse x < 0
    [set d_shop 2]
    [set d_shop 3]

    ifelse x < 0
    [set d_school 3]
    [set d_school 4]
  ]
  ;; if on a vertical road (doesn't include intersection since this was accounted for in above statement)
  if x = 0 and y != 0 [
    ifelse y < 0
    [set d_school 1]
    [set d_school 2]
    ifelse y < 0
    [set d_shop 2]
    [set d_shop 1]
    ifelse y < 0
    [set d_work 2]
    [set d_work 3]
    ifelse y < 0
    [set d_house 3]
    [set d_house 4]

  ]

end

;; this sets all of the different distances form a pedestrian current location to the beginning of a destination road
;; it incorporates the fact that these are one way streets
to check-d-person
  ;; this will set distance values for each location - values can be 1-4 depending on order of how far each locations road is
    let x [pxcor] of patch-here
    let y [pycor] of patch-here

  ;; if on horizontal road
  if y = -2 [
    ifelse x < 0
    [set d_house 1]  ;; if you are currently on the house road
    [set d_house 2]  ;; if you are currently on the work road

    ifelse x < 0
    [set d_work 2]      ;; if you are currently on the house road
    [set d_work 1]      ;; if you are currently on the work road

    ifelse x < 0
    [set d_shop 2]
    [set d_shop 3]

    ifelse x < 0
    [set d_school 3]
    [set d_school 4]
  ]
  ;; if on a vertical road (doesn't include intersection since this was accounted for in above statement)
  if x = -2 and y != -2 [
    ifelse y < 0
    [set d_school 1]
    [set d_school 2]
    ifelse y < 0
    [set d_shop 2]
    [set d_shop 1]
    ifelse y < 0
    [set d_work 2]
    [set d_work 3]
    ifelse y < 0
    [set d_house 3]
    [set d_house 4]

  ]

end

;; if parking time is up, move back to the road (for HVs)
to move-back-to-road
  let canMove false

    ;; checks all parking lists to find which destination it is parked at
    ;; case 1: it parks at shop
    if member? patch-here list-parking-shop
    [
      let x 0
      let y [pycor] of patch-here
    ;; first check if there is an opening in road for the car to enter
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        ask patch-here [set occupied? false] ;; parking spot is now available
        setxy x y
        set heading 0
        set from shop

      ifelse money - 5 < 0
      [set money 0]
      [set money money - 5]
      ;; updates money since HV was just shopping                                                           needfix
      ifelse  family - 4 < 0
      [set family 0]
      [set family family - 4]
      ifelse  education - 4 < 0
      [set education 0]
      [set education education - 4]
      ifelse materialism > max-materialism
      [set materialism max-materialism ]
      [set materialism materialism + 10 ]

        set canMove true
      ]
    ]
    ;; case 2: it parks at work
    if member? patch-here list-parking-work
    [
      let x [pxcor] of patch-here
      let y 0
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        ask patch-here [set occupied? false]
        setxy x y

        set heading 90
        set from work
        set money money + 10 ;; updates money since HV was just working
        set family family - 2 ;; takes away from family time
        set materialism materialism - 2
        set education education + 0
        set canMove true
      ]
    ]
    ;; case 3: it parks at house
    if member? patch-here list-parking-house
    [
      let x [pxcor] of patch-here
      let y 0
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        ask patch-here [set occupied? false]
        setxy x y
        set heading 90
        set from house
        set family family + 10
        set materialism materialism - 4
        set money money + 0
        set education education + 0
        set canMove true
      ]
    ]
    ;; case 4: it parks at school
    if member? patch-here list-parking-school
    [
      let x 0
      let y [pycor] of patch-here
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        ask patch-here [set occupied? false]
        setxy x y
        set heading 0
        set from school
        set education education + 10
        set materialism materialism - 4
        set money money - 4
        set family family + 0
        set canMove true
      ]
    ]

  if money > max-money
  [set money max-money]
  if money < 0
  [set money 0]
  if family > max-family
  [set family max-family]
  if family < 0
  [set family 0]
 if education > max-education
  [set education max-education]
 if education < 0
  [set education 0]
 if materialism > max-materialism
  [set materialism max-materialism]
 if materialism < 0
  [set materialism 0]

  ;; update emotion levels
  if money > max-money or money < 0
  [set emotion_level emotion_level + 4]

  if family > max-family or family < 0
  [set emotion_level emotion_level + 3]

  if education > max-education or education < 0
  [set emotion_level emotion_level + 2]

  if materialism > max-materialism or materialism < 0
  [set emotion_level emotion_level + 1]

  ;; sets a new goal destination - decide new destination
  if canMove [
    ;set goto one-of goal-candidates with [ self != [ goto ] of myself ]
    set goto destination-choice-HV goto
    set on? true
  ]
end

;; if time is up, walk back to the road (for pedestrian)
to walk-back-to-road
  let canMove false

  ;; shop
    if [pxcor] of patch-here = -3 and [pycor] of patch-here = 11
    [
      let x -2
      let y (random 7) + 11
    ;; first check if there is an opening in road for the car to enter
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        setxy x y
        set heading 0
        set from one-of shop

      ifelse money - 5 < 0
      [set money 0]
      [set money money - 5]
      ifelse  family - 4 < 0
      [set family 0]
      [set family family - 4]
      ifelse  education - 4 < 0
      [set education 0]
      [set education education - 4]
      ifelse materialism > max-materialism
      [set materialism max-materialism ]
      [set materialism materialism + 10 ]

        set canMove true
      ]
    ]
    ;; work
    if [pxcor] of patch-here = 11 and [pycor] of patch-here = 3
    [
      let x (random 7) + 11
      let y -2
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        setxy x y
        set heading 90
        set from one-of work

        set money money + 10 ;; updates money since HV was just working
        set family family - 2 ;; takes away from family time
        set materialism materialism - 2
        set education education + 0
        set canMove true
      ]
    ]

   ;; house
    if [pxcor] of patch-here = -9 and [pycor] of patch-here = -3
    [
      let x (random 6) - 9
      let y -2
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        setxy x y
        set heading 90
        set from one-of house
        set family family + 10
        set materialism materialism - 4
        set money money + 0
        set education education + 0
        set canMove true
      ]
    ]
  ;; school
  if [pxcor] of patch-here = 3 and [pycor] of patch-here = -11
    [
      let x -2
      let y (random 6) - 9
      if count [turtles-here] of one-of patches with [pxcor = x and pycor = y] = 0 [
        setxy x y
        set heading 0
        set from one-of school
        set education education + 10
        set materialism materialism - 4
        set money money - 4
        set family family + 0
        set canMove true
      ]
    ]

  if money > max-money
  [set money max-money]
  if money < 0
  [set money 0]
  if family > max-family
  [set family max-family]
  if family < 0
  [set family 0]
 if education > max-education
  [set education max-education]
 if education < 0
  [set education 0]
 if materialism > max-materialism
  [set materialism max-materialism]
 if materialism < 0
  [set materialism 0]

  ;; update emotion levels
  if money > max-money or money < 0
  [set emotion_level emotion_level + 4]

  if family > max-family or family < 0
  [set emotion_level emotion_level + 3]

  if education > max-education or education < 0
  [set emotion_level emotion_level + 2]

  if materialism > max-materialism or materialism < 0
  [set emotion_level emotion_level + 1]

  ;; sets a new goal destination - decide new destination
  if canMove [
    ;set goto one-of goal-candidates with [ self != [ goto ] of myself ]
    set goto destination-choice-HV goto
    set on? true
  ]
end

;; emotional decay
to decay-emotion
  set emotion_level emotion_level * exp -0.9
end

;; sets up parking count down
to set-counter
    set count-down 200 ;; set parking time
end

;; sets up parking count down
to set-counter-pedestrian
    set count-down 200 ;; set parking time
end

;; to decrement time count down
to decrement-counter
    set count-down count-down - 1
end


; Copyright 2017 Zhiyang Chen, Bari Gordon, Meiqing Li.
; See Info tab for documentation.
@#$#@#$#@
GRAPHICS-WINDOW
15
15
578
579
-1
-1
15.0
1
20
1
1
1
0
1
1
1
-18
18
-18
18
1
1
1
ticks
30.0

PLOT
960
15
1300
220
Number of Agents
Time
Number of Agents
0.0
50.0
0.0
50.0
true
true
"" ""
PENS
"HVs" 1.0 0 -1184463 true "" "plot count turtles with [breed = HVs]"
"AVs" 1.0 0 -14730904 true "" "plot count turtles with [breed = AVs]"
"Pedestrians" 1.0 0 -2064490 true "" "plot count turtles with [breed = People]"

SLIDER
790
140
950
173
num-car-users
num-car-users
1
400
103.0
1
1
NIL
HORIZONTAL

PLOT
960
230
1300
450
Stopped Agents
Time
Stopped Agents
0.0
100.0
0.0
100.0
true
true
"set-plot-y-range 0 (total-num-cars + num-pedestrians)" ""
PENS
"Stopped AVs" 1.0 0 -14730904 true "" "plot count turtles with [(speed = 0) and (breed = AVs)]"
"Stopped HVs" 1.0 0 -1184463 true "" "plot count turtles with [(speed = 0) and (breed = HVs)]"
"Stopped Peds" 1.0 0 -2064490 true "" "plot count turtles with [(speed = 0) and (breed = People)]"

BUTTON
610
75
695
120
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
610
15
694
60
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
790
70
945
103
ticks-per-cycle
ticks-per-cycle
1
100
93.0
1
1
NIL
HORIZONTAL

SLIDER
610
185
780
218
percentage-of-AVs
percentage-of-AVs
0
100
16.0
1
1
%
HORIZONTAL

SLIDER
610
140
780
173
parking-ratio
parking-ratio
0
100
23.0
1
1
%
HORIZONTAL

MONITOR
1415
385
1525
430
Number of HVs
num-HVs
0
1
11

SLIDER
790
25
945
58
current-phase
current-phase
0
99
8.0
1
1
%
HORIZONTAL

PLOT
1310
15
1690
215
Average Values Factors (HVs)
NIL
NIL
0.0
10.0
15.0
40.0
true
true
"" ""
PENS
"Money" 1.0 0 -15040220 true "" "plot mean [money] of HVs"
"Family" 1.0 0 -955883 true "" "plot mean [family] of HVs"
"Education" 1.0 0 -14070903 true "" "plot mean [education] of HVs"
"Materialism" 1.0 0 -5825686 true "" "plot mean [materialism] of HVs"

SLIDER
790
185
950
218
num-pedestrians
num-pedestrians
0
100
30.0
1
1
NIL
HORIZONTAL

PLOT
960
465
1300
670
Average time spent to get to destination
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"AVs" 1.0 0 -14730904 true "" "plot mean [time-to-dest-AV] of AVs"
"HVs" 1.0 0 -1184463 true "" "plot mean [time-to-dest-HV] of HVs"
"Peds" 1.0 0 -2064490 true "" "plot mean [time-to-dest-people] of People"

PLOT
1310
230
1690
445
Average Value Factors (Pedestrians)
NIL
NIL
0.0
10.0
15.0
40.0
true
true
"" ""
PENS
"Money" 1.0 0 -15575016 true "" "plot mean [money] of People"
"Family" 1.0 0 -955883 true "" "plot mean [family] of People"
"Education" 1.0 0 -15390905 true "" "plot mean [education] of People"
"Materialism" 1.0 0 -10022847 true "" "plot mean [materialism] of People"

SLIDER
610
245
780
278
high-money-need-HV
high-money-need-HV
0
40
20.0
1
1
NIL
HORIZONTAL

SLIDER
790
245
950
278
med-money-need-HV
med-money-need-HV
0
40
30.0
1
1
NIL
HORIZONTAL

SLIDER
610
285
780
318
high-family-need-HV
high-family-need-HV
0
40
18.0
1
1
NIL
HORIZONTAL

SLIDER
790
285
950
318
med-family-need-HV
med-family-need-HV
0
40
26.0
1
1
NIL
HORIZONTAL

SLIDER
610
325
780
358
high-education-need-HV
high-education-need-HV
0
40
18.0
1
1
NIL
HORIZONTAL

SLIDER
790
325
950
358
med-education-need-HV
med-education-need-HV
0
40
26.0
1
1
NIL
HORIZONTAL

SLIDER
610
365
780
398
high-materialism-need-HV
high-materialism-need-HV
0
40
12.0
1
1
NIL
HORIZONTAL

SLIDER
790
365
950
398
med-materialism-need-HV
med-materialism-need-HV
0
40
20.0
1
1
NIL
HORIZONTAL

SLIDER
610
425
780
458
high-need-money-Ped
high-need-money-Ped
0
40
15.0
1
1
NIL
HORIZONTAL

SLIDER
790
425
950
458
med-need-money-Ped
med-need-money-Ped
0
40
25.0
1
1
NIL
HORIZONTAL

SLIDER
610
465
780
498
high-need-family-Ped
high-need-family-Ped
0
40
12.0
1
1
NIL
HORIZONTAL

SLIDER
790
465
950
498
med-need-family-Ped
med-need-family-Ped
0
40
20.0
1
1
NIL
HORIZONTAL

SLIDER
610
505
780
538
high-need-education-Ped
high-need-education-Ped
0
40
12.0
1
1
NIL
HORIZONTAL

SLIDER
790
505
950
538
med-need-education-Ped
med-need-education-Ped
0
40
20.0
1
1
NIL
HORIZONTAL

SLIDER
610
545
780
578
high-need-materialism-Ped
high-need-materialism-Ped
0
40
8.0
1
1
NIL
HORIZONTAL

SLIDER
790
545
950
578
med-need-materialism-Ped
med-need-materialism-Ped
0
40
13.0
1
1
NIL
HORIZONTAL

PLOT
15
590
300
745
Average Emotion
NIL
NIL
0.0
200.0
0.0
6.0
true
true
"" ""
PENS
"HVs" 1.0 0 -1184463 true "" "plot mean [emotion_level] of HVs"
"People" 1.0 0 -2064490 true "" "plot mean [emotion_level] of People"

PLOT
1310
465
1690
670
Average Congestion on Roads (cars)
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Work" 1.0 0 -16777216 true "" "plot count HVs-on work-road + count AVs-on work-road"
"House" 1.0 0 -7500403 true "" "plot count HVs-on house-road + count AVs-on house-road"
"Shop" 1.0 0 -2674135 true "" "plot count HVs-on shop-road + count AVs-on shop-road"
"School" 1.0 0 -955883 true "" "plot count HVs-on school-road + count AVs-on school-road"

PLOT
610
590
950
745
Average Wait Time
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [wait-time] of turtles with [breed = AVs or breed = HVs]"

PLOT
310
590
580
745
Number of jaywalkers (people and HVs)
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count turtles with [jaywalk]"

@#$#@#$#@
## ACKNOWLEDGMENT

This model is partially based on the example from Chapter Five of the book "Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo", by Uri Wilensky & William Rand.

* Wilensky, U. & Rand, W. (2015). Introduction to Agent-Based Modeling: Modeling Natural, Social and Engineered Complex Systems with NetLogo. Cambridge, MA. MIT Press.

## WHAT IS IT?

The Autonomous Vehicle, Destination and Safety Utility at Intersection model simulates traffic moving in a simplified city grid with two perpendicular one-way streets that forms an intersection. It allows you to control global variables, such as the AV/HV ratio, the number of car users and pedestrians, and explore traffic dynamics, particularly traffic congestion measured by average waiting time, and safety conditions such as number of jaywalkers and number of agents in simulation.

This model gives the cars destinations (house, shop, school, work). The agents in this model use goal-based and adaptive cognition.

## HOW IT WORKS

Each time step, the agents (AVs, HVs, and Pedestrians) face the next destination they are trying to get to (work , house, shop or school). When at an intersection, HVs and Pedestrians will have to decide whether to prioritize safety or reaching their destination. This will be done through a utility function that takes into account the level of need to get to destination, the color of the light at the intersection, if there are other cars at the intersection, emotional levels, and if there are foes ahead. 

In each cycle, each car will be assigned one destination. Traffic lights at the intersection will automatically change at the beginning of each cycle.

## HOW TO USE IT

Change any setting that you would like to change. Press the SETUP button.

Start the simulation by pressing the GO button. 

### Buttons

SETUP --  sets up patches and parking spots for each destination. Parking spots are randomly allocated based on parking ratio. This button also places AVs and HVs on the street.
GO -- runs the simulation indefinitely. Cars travel from their homes to their work and back.

### Sliders

NUM-CAR_USERS -- sets the number of car users in the simulation.

NUM_PEDESTRIANS -- sets the number of pedestrians in the simulation

PERCENTAGE-OF-AVS -- percentage of AVs in total number of cars

PARKING-RATIO -- percentage of HVs that have a parking spot

TICKS-PER-CYCLE -- sets the number of ticks that will elapse for each cycle. This has no effect on manual lights. This allows you to increase or decrease the granularity with which lights can automatically change.

CURRENT-PHASE -- controls when the current light changes, if it is in auto mode. The slider value represents the percentage of the way through each cycle at which the light should change. So, if the TICKS-PER-CYCLE is 20 and CURRENT-PHASE is 75%, the current light will switch at tick 15 of each cycle.

High-money-need-HV -- sets the threshold for what level of money constitutes a high need for destination for an HV

Med-money-need-HV -- sets the threshold for what level of money constitutes a medium need for destination for an HV

^^ sliders similar to these above two for all other value factors (i.e. famility, eduction and materialism) and for pedestrians too.

### Plots

Average Emotions -- displays the average emotion level for HVs and Pedestrians.

STOPPED Agents -- displays the number of stopped agents over time.

Number of Agents -- displays the number of HVs, AVs, and Pedestrians over time.

Average Values Factors (HVs) - displays the average level of money, materialism, education, and family for HVs over time.

Average Values Factors (Pedestrians) - displays the average level of money, materialism, education, and family for Pedestrians over time.

Average Congestion on Roads (cars) -- displays the number of cars on each destination road.

Average time spent to get to destination -- displays the average time it takes for each agent to reach their destinations.

AVERAGE WAIT TIME -- displays the average time cars are stopped over time (excluding parked cars).

Number of jaywalkers -- displays the number of HVs and Pedestrians breaking traffic laws
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
true
0
Polygon -7500403 true true 180 15 164 21 144 39 135 60 132 74 106 87 84 97 63 115 50 141 50 165 60 225 150 285 165 285 225 285 225 15 180 15
Circle -16777216 true false 180 30 90
Circle -16777216 true false 180 180 90
Polygon -16777216 true false 80 138 78 168 135 166 135 91 105 106 96 111 89 120
Circle -7500403 true true 195 195 58
Circle -7500403 true true 195 47 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
