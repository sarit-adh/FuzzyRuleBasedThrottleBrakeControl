;Expert System program for autonomous vehicle navigation.
;CS 514 Applied Artificial Intelligence
;project 2
;author : skynet

;extension from project 1

;watch all tells Jess to print some useful diagnostics
;reset commands resets runtime memory
;(watch all)
(reset)


;import from fuzzy packages
(import nrc.fuzzy.*)
(import nrc.fuzzy.jess.*)
(load-package nrc.fuzzy.jess.FuzzyFunctions)


;global variables
;input
(defglobal ?*speedFvar* = (new FuzzyVariable "speed" 0.0 100.0 "miles/hour"))
(defglobal ?*distanceToObstacleFvar* = (new FuzzyVariable "distance to obstacle" 0.0 200.0 "feet"))

;output
;complete press of throttle/brake denoted by 1.0 and no press of throttle/brake denoted by 0.0
(defglobal ?*throttleFvar* = (new FuzzyVariable "throttle press" 0.0 1.0 ""))
(defglobal ?*brakeFvar* = (new FuzzyVariable "brake press" 0.0 1.0 ""))




;TEMPLATES
(deftemplate vehicle
	"The template for vehicle agent"
	(slot isRunning)(slot type)(slot height)(slot gear)(slot speed)(slot throttle)(slot brake))

(deftemplate obstacle
    (slot speed)(slot distance))



;RULES
(defrule welcome-message "this is the entry point of the system"
  (declare (salience 10))
  =>
  
  (?*speedFvar* addTerm "none" (new SingletonFuzzySet 0))
  (?*speedFvar* addTerm "slow" (new TrapezoidFuzzySet 0.0 5.0 30.0 35.0))
  (?*speedFvar* addTerm "medium" (new PIFuzzySet 50 20))  
  (?*speedFvar* addTerm "fast" (new SFuzzySet 45 100.0))
  
  (?*throttleFvar* addTerm "none" (new ZFuzzySet 0 0.05 ))
  (?*throttleFvar* addTerm "low" (new TriangleFuzzySet 0.0 .15 .25))
  (?*throttleFvar* addTerm "medium" (new TriangleFuzzySet .15 .35 .55))  
  (?*throttleFvar* addTerm "high" (new SFuzzySet .45 .7))
  
  (?*brakeFvar* addTerm "none" (new ZFuzzySet 0 0.05  ))
  (?*brakeFvar* addTerm "low" (new TriangleFuzzySet 0.0 .15 .25))
  (?*brakeFvar* addTerm "medium" (new TriangleFuzzySet .15 .35 .55))  
  (?*brakeFvar* addTerm "high" (new SFuzzySet .45 .7 ))
    
  (?*distanceToObstacleFvar* addTerm "near" (new TrapezoidFuzzySet 0.0 10.0 60.0 70.0))
  (?*distanceToObstacleFvar* addTerm "medium" (new PIFuzzySet 100 60))  
  (?*distanceToObstacleFvar* addTerm "far" (new SFuzzySet 100 200.0))        
      
    
  (assert (vehicle(isRunning "FALSE")(type "car")(height 7)(gear "park")
            (speed (new FuzzyValue ?*speedFvar*(new SingletonFuzzySet 0)))
            (throttle (new FuzzyValue ?*throttleFvar*(new SingletonFuzzySet 0)))
            (brake (new FuzzyValue ?*brakeFvar*(new SingletonFuzzySet 0)))))
    
  ;(assert(obstacle
  ;  (speed (new FuzzyValue ?*speedFvar* (new TriangleFuzzySet 60 70 80)))
  ;  (distance(new FuzzyValue ?*distanceToObstacleFvar* (new TriangleFuzzySet 110 120 130)))
  ;  ))
      
  (printout t crlf crlf)    
  (printout t "###########################################" crlf)  
  (printout t "Welcome to autonomous driving expert system" crlf)
  (printout t "###########################################" crlf)  
  (printout t crlf crlf)
  ;(printout t "Good to have you on board.Please set your destination to start your ride" crlf)
  ;(bind ?destination (read) crlf)
  (assert (destination-fixed "chicago"))
    
  (printout t "Enter speed of obstacle")
  (bind ?speed-value (float (readline t)))
    
  (printout t "Enter distance between vehicle and obstacle")
  (bind ?distance-value (float (readline t)))
    
  (assert(obstacle
    (speed (new FuzzyValue ?*speedFvar* (new TriangleFuzzySet (- ?speed-value 0.25) ?speed-value (+ ?speed-value 0.25))))
    (distance(new FuzzyValue ?*distanceToObstacleFvar* (new TriangleFuzzySet (- ?distance-value 0.5) ?distance-value (+ ?distance-value 0.5))))
            
    ))    
    
  
  
      
  )




(defrule check-destination-reachable
    "This rule checks if the destination is reachable"
    (destination-fixed ?destination)
	=>
    (printout t "The destination " ?destination " is reachable" crlf)
    (assert (destination-reachable))
    
    )

(defrule alternative-route-rule
	"When destination is unreachable"
    (not(destination-reachable)) ?arr_fact <- (alternative-route-required)
	=>
    (printout t "The destination is not reachable, navigating to alternate route" crlf)
    (assert(destination-reachable))
    (retract ?arr_fact)
    )

(defrule check-vehicle-condition
	"this rule checks if the vehicle is in working condition"
    (destination-reachable)
    (not (no-gas))
    (not (battery-dead))
    (not (alternator-damaged))
    (not (starter-damaged))
    (not (frozen-fuel-line))
    (not (spark-plugs-damaged))
	=>    
    (assert(vehicle-working))
    )

(defrule no-gas-rule
	"No gas"
    (no-gas)
	=>
    (printout t "vehicle cannot start , please refill the gas ")
    )

(defrule battery-dead-rule
	"Battery Dead"
    (battery-dead)
	=>
    (printout t "vehicle cannot start , please replace the battery ")
    )

(defrule alternator-damaged-rule
	"Alternator Damaged"
    (alternator-damaged)
	=>
    (printout t "vehicle cannot start , please repair the alternator ")
    )

(defrule starter-damaged-rule
	"Starter Damaged"
    (starter-damaged)
	=>
    (printout t "vehicle cannot start , please repair the starter ")
    )

(defrule frozen-fuel-line-rule
	"Frozen Fuel Line"
    (frozen-fuel-line)
	=>
    (printout t "vehicle cannot start , please check the fuel line ")
    )

(defrule spark-plugs-damaged-rule
	"Spark Plugs Damaged"
    (frozen-fuel-line)
	=>
    (printout t "vehicle cannot start , please repair the spark plugs ")
    )

(defrule prepare-vehicle-navigation
	"when destination is fixed and is reachable the vehicle attempts to start navigation"
    (declare (salience 9))
    (vehicle-working) 
    ?cur_vehicle <- (vehicle(type ?type)(isRunning "FALSE"))
    (garage-open)
    (not(alternative-route-required))
	=>
    (printout t "The " ?type " is in running condition, getting out of garage" crlf)    
    (assert(vehicle-ready-for-navigation))
    
    
    )

(defrule start-vehicle-navigation
	"when car is out of garage , start the navigation" 
    ?cur_vehicle <- (vehicle(type ?type)(isRunning "FALSE")(speed ?speed))
    (vehicle-ready-for-navigation)
	=>
    
    
    ;(modify ?cur_vehicle(speed fval(new FuzzyValue(new TriangleFuzzySet(40,5)))))
    (modify ?cur_vehicle(speed(new FuzzyValue ?*speedFvar* "fast")))
    (modify ?cur_vehicle(gear "drive"))
    (modify ?cur_vehicle(isRunning "TRUE"))
    
    (printout t "The " ?type " is running"  crlf)
      
  
    )



(defrule height-limit-sign-detect
	"when detected the sign that limits height when passing through underpass"
    ?cur_vehicle <- (vehicle{height > 5}(isRunning TRUE)) ?hls_fact <- (height-limit-sign) 
    ?dest_reachable_fact <- (destination-reachable)
	=>
    (printout t "height limit sign detected, cannot pass through underpass" crlf)
    
    ;(modify ?cur_vehicle(isRunning FALSE) )
    ;(modify ?cur_vehicle(speed 0) )
    
    (assert(alternative-route-required))
    
    (retract ?dest_reachable_fact)
    (retract ?hls_fact)
    )

(defrule road-closure-detect
	"when road is closed, stop the vehicle and prompt to find alternate route"
    ?cur_vehicle <- (vehicle{height > 5}(isRunning TRUE)) ?rc_fact <- (road-closed) 
    ?dest_reachable_fact <- (destination-reachable)
	=>
    (printout t "Road closed, cannot continue in current route" crlf)
 	
    (assert(alternative-route-required))
    
    (retract ?dest_reachable_fact)
    (retract ?rc_fact)
    )

(defrule school-bus-detect-rule
	"when vehicle is running above speed limit, slow down"
    ?cur_vehicle <- (vehicle(speed ?s&:(fuzzy-match ?s "fast"))(isRunning "TRUE"))?sbd_fact <- (school-bus-detect) 
	=>
    (printout t "school ahead sign detected, slowing down" crlf)
    (modify ?cur_vehicle(speed(new FuzzyValue ?*speedFvar* "slow")))
    (retract ?sbd_fact)
    )

(defrule intersection-ahead
	"when intersection ahead sign is detected"
    ?cur_vehicle <- (vehicle{speed > 20}(isRunning TRUE)) ?ias_fact <- (intersection-ahead-sign) 
	=>
    (printout t "intersection sign detected, slowing down" crlf)
    (modify ?cur_vehicle(speed 20))
    (assert(ready-for-intersection))
    (retract ?ias_fact)
    )

(defrule intersection-turn-attempt-clear
	"comment"
    ?rfi_fact<-(ready-for-intersection) ?tsg_fact<- (traffic-signal-green)
    ?nvi_fact<- (no-vehicles-in-intersection)
	=>
    (printout t "safe to turn through intersection, turning now" crlf)
    (retract ?nvi_fact)
    (retract ?tsg_fact)
    (retract ?rfi_fact)
    
    )

(defrule intersection-turn-attempt-lights-not-green
	"comment"
    (ready-for-intersection)(not(traffic-signal-green))
	=>
    (printout t "traffic signal red, waiting...." crlf)
    )

(defrule intersection-turn-attempt-vehicles-in-intersection
	"comment"
    (ready-for-intersection)(traffic-signal-green)(not(no-vehicles-in-intersection))
	=>
    (printout t "vehicles in intersection, waiting..." crlf)
    )


(defrule obstacle-slow-vehicle-detect
	"when a vehicle detects any obstacle of slow vehicle"
    (vehicle-ahead)(vehicle-ahead-too-slow)(vehicle(isRunning TRUE))
	=>
    (printout t "slow vehicle detected in current lane" crlf)
    (assert(obstacle-ahead)))



(defrule prepare-change-of-lanes
	"when slow vehicle is ahead and not willing to change lane"
    (obstacle-ahead)(not(obstacle-changing-lane-to-right))(vehicle(isRunning TRUE))
	=>
    (printout t "preparing for left lane change" crlf)
    (assert(want-left-lane-change))
	)

(defrule continue-in-the-current-lane
	"when slow vehicle is ahead and is willing to change lane"
    (obstacle-ahead)(obstacle-changing-lane-to-right)
	=>
    (printout t "slow vehicle ahead is changing lane, so continuing on current lane" crlf))

(defrule change-to-left-lane
	"when left lane is clear change lanes "
    (want-left-lane-change)(left-lane-change-safe)
	=>
    (printout t "changing to left lane is safe, switching lanes" crlf))

; throttle, break control fuzzy rules start

 (defrule vehicle-slow-obstacle-slow-near (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "slow"))
    (distance ?d&:(fuzzy-match ?d "near"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "slow"))
        )   
    =>
    (printout t "vehicle-slow-obstacle-slow-near rule triggered" crlf)
    
    
    )
(defrule vehicle-medium-obstacle-slow-near (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "slow"))
    (distance ?d&:(fuzzy-match ?d "near"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "medium"))
        )   
    =>
    (printout t "vehicle-medium-obstacle-slow-near rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "low")))
    
    )
(defrule vehicle-fast-obstacle-slow-near (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "slow"))
    (distance ?d&:(fuzzy-match ?d "near"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "fast"))
        )   
    =>
    (printout t "vehicle-fast-obstacle-slow-near rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "high")))
    
    )
(defrule vehicle-slow-obstacle-medium-near (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "medium"))
    (distance ?d&:(fuzzy-match ?d "near"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "slow"))
        )   
    =>
    (printout t "vehicle-slow-obstacle-medium-near rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "low"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-medium-obstacle-medium-near (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "medium"))
    (distance ?d&:(fuzzy-match ?d "near"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "medium"))
        )   
    =>
    (printout t "vehicle-medium-obstacle-medium-near rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-fast-obstacle-medium-near (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "medium"))
    (distance ?d&:(fuzzy-match ?d "near"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "fast"))
        )   
    =>
    (printout t "vehicle-fast-obstacle-medium-near rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "medium")))
    
    )
(defrule vehicle-slow-obstacle-fast-near (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "fast"))
    (distance ?d&:(fuzzy-match ?d "near"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "slow"))
        )   
    =>
    (printout t "vehicle-slow-obstacle-fast-near rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "low"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-medium-obstacle-fast-near (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "fast"))
    (distance ?d&:(fuzzy-match ?d "near"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "medium"))
        )   
    =>
    (printout t "vehicle-medium-obstacle-fast-near rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "low"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-fast-obstacle-fast-near (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "fast"))
    (distance ?d&:(fuzzy-match ?d "near"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "fast"))
        )   
    =>
    (printout t "vehicle-fast-obstacle-fast-near rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-slow-obstacle-slow-medium (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "slow"))
    (distance ?d&:(fuzzy-match ?d "medium"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "slow"))
        )   
    =>
    (printout t "vehicle-slow-obstacle-slow-medium rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "low"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-medium-obstacle-slow-medium (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "slow"))
    (distance ?d&:(fuzzy-match ?d "medium"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "medium"))
        )   
    =>
    (printout t "vehicle-medium-obstacle-slow-medium rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "low")))
    
    )
(defrule vehicle-fast-obstacle-slow-medium (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "slow"))
    (distance ?d&:(fuzzy-match ?d "medium"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "fast"))
        )   
    =>
    (printout t "vehicle-fast-obstacle-slow-medium rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "high")))
    
    )
(defrule vehicle-slow-obstacle-medium-medium (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "medium"))
    (distance ?d&:(fuzzy-match ?d "medium"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "slow"))
        )   
    =>
    (printout t "vehicle-slow-obstacle-medium-medium rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "low"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-medium-obstacle-medium-medium (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "medium"))
    (distance ?d&:(fuzzy-match ?d "medium"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "medium"))
        )   
    =>
    (printout t "vehicle-medium-obstacle-medium-medium rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-fast-obstacle-medium-medium (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "medium"))
    (distance ?d&:(fuzzy-match ?d "medium"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "fast"))
        )   
    =>
    (printout t "vehicle-fast-obstacle-fast-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "medium")))
    
    )
(defrule vehicle-slow-obstacle-fast-medium (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "fast"))
    (distance ?d&:(fuzzy-match ?d "medium"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "slow"))
        )   
    =>
    (printout t "vehicle-slow-obstacle-fast-medium rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "high"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-medium-obstacle-fast-medium (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "fast"))
    (distance ?d&:(fuzzy-match ?d "medium"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "medium"))
        )   
    =>
    (printout t "vehicle-medium-obstacle-fast-medium rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "medium"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-fast-obstacle-fast-medium (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "fast"))
    (distance ?d&:(fuzzy-match ?d "medium"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "fast"))
        )   
    =>
    (printout t "vehicle-fast-obstacle-fast-medium rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-slow-obstacle-slow-far (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "slow"))
    (distance ?d&:(fuzzy-match ?d "far"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "slow"))
        )   
    =>
    (printout t "vehicle-slow-obstacle-slow-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "high"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-medium-obstacle-slow-far (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "slow"))
    (distance ?d&:(fuzzy-match ?d "far"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "medium"))
        )   
    =>
    (printout t "vehicle-medium-obstacle-slow-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-fast-obstacle-slow-far (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "slow"))
    (distance ?d&:(fuzzy-match ?d "far"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "fast"))
        )   
    =>
    (printout t "vehicle-fast-obstacle-fast-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "none"))
          (change_brake (new FuzzyValue ?*brakeFvar* "medium")))
    
    )
(defrule vehicle-slow-obstacle-medium-far (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "medium"))
    (distance ?d&:(fuzzy-match ?d "far"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "slow"))
        )   
    =>
    (printout t "vehicle-slow-obstacle-medium-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "high"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-medium-obstacle-medium-far (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "medium"))
    (distance ?d&:(fuzzy-match ?d "far"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "medium"))
        )   
    =>
    (printout t "vehicle-medium-obstacle-medium-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "low"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-fast-obstacle-medium-far (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "medium"))
    (distance ?d&:(fuzzy-match ?d "far"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "fast"))
        )   
    =>
    (printout t "vehicle-fast-obstacle-medium-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "low"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-slow-obstacle-fast-far (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "fast"))
    (distance ?d&:(fuzzy-match ?d "far"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "slow"))
        )   
    =>
    (printout t "vehicle-slow-obstacle-fast-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "high"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-medium-obstacle-fast-far (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "fast"))
    (distance ?d&:(fuzzy-match ?d "far"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "medium"))
        )   
    =>
    (printout t "vehicle-medium-obstacle-fast-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "high"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
(defrule vehicle-fast-obstacle-fast-far (declare (salience -10))
    (obstacle
    (speed ?s&:(fuzzy-match ?s "fast"))
    (distance ?d&:(fuzzy-match ?d "far"))
    )
    (vehicle
     (speed ?vs&:(fuzzy-match ?vs "fast"))
        )   
    =>
    (printout t "vehicle-fast-obstacle-fast-far rule triggered" crlf)
    
    (assert (change_throttle (new FuzzyValue ?*throttleFvar* "low"))
          (change_brake (new FuzzyValue ?*brakeFvar* "none")))
    
    )
   
; throttle, break control fuzzy rules end


(defrule defuzzify "low salience to allow all rules to fire before defuzzifying"
   (declare (salience -100))
  ?tf <- (change_throttle ?t)
  ?bf <- (change_brake ?b)
  ?vehicle <- (vehicle(speed ?speed))
  ?obstacle <- (obstacle)
 =>
  (bind ?throttle-change (?t momentDefuzzify))
  (bind ?brake-change (?b momentDefuzzify))  
  ;(bind ?old-speed ( ?speed momentDefuzzify))  
  ;(store COLDVALVECHANGE ?cold-change)
  ;(store HOTVALVECHANGE ?hot-change)
  ;(store RULESTHATFIRED ?*rulesThatFired*)
  ;(bind ?*rulesThatFired* "")
  
  ;(printout t "previous speed set to: " ?old-speed crlf)  
      
  (printout t "throttle set to: " ?throttle-change crlf)
  (printout t "brake set to: " ?brake-change crlf)
        
  (retract ?tf ?bf)
)

; The facts for different scenarios are enlisted below.
; But, Most of them have been commented out initially.
; You might want to test each scenario by uncommenting 
; the line including the fact
;
; Please refer to the documentaion for elaborate description
; for each of the scenario
; 
;
;

;FACTS

;TEST FACT 1

(assert(garage-open))
;(assert(no-gas))


;TEST FACT 2 height limit underpass
;(assert (height-limit-sign))

;TEST FACTS 3 changing lanes
;(assert (vehicle-ahead))
;(assert(vehicle-ahead-too-slow))


;When following line is commented out, the vehicle prepares the left lane change and when the fact
;exists, the vehicle continues on it's path
;(assert (obstacle-changing-lane-to-right))


;When left lane change is safe i.e. vehicle is not approaching
;(assert(left-lane-change-safe))

;TEST FACT 4 road closed
;(assert(road-closed))


;TEST FACT 5 school bus detect
;(assert(school-bus-detect))
(assert(vehicle-ahead))

;TEST FACT 6 intersection
;(assert(intersection-ahead-sign))
;(assert(traffic-signal-green))
;(assert(no-vehicles-in-intersection))

;TEST FACT 7 obstacle-far-speed fast 
;(assert(obstacle
;    (speed (new FuzzyValue ?*speedFvar* (new TriangleFuzzySet 60 70 80)))
;    (distance(new FuzzyValue ?*distanceToObstacleFvar* (new TriangleFuzzySet 110 120 130)))
;    ))

;(assert(obstacle
;    (speed (new FuzzyValue ?*speedFvar* "slow"))
;    (distance(new FuzzyValue ?*distanceToObstacleFvar* "far"))))

;Jess rules only fire while the rule engine is running
;(although they can be activated while the engine is not running.) 
;To start the engine running, we issue the run command.

(run)

(printout t crlf crlf)
(facts)