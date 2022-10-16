# Example 1: Demonstrate several Tasks tricks and run with the vw_debugger
#
#   In this example, we have the main thread sending jobs to the fibsize 2-task group.
#   The code demonstrates how one can retreive the current queue size. In so doing,
#   the result is that the queue size will never exceed max+10 = 25.
#
#   By using -tasks -2 we get 2 tasks, but also a callback as each job finishes. The callback
#   uses the same name as the Tproc, so it's  also called fibsize. Note, it must be defined after the
#   Tproc command, and before any tgroup commands. This is because the Tproc defines a proc
#   fibsize also (but we don't need it in the main thread). It must be before any calls to
#   tgroup is made to start jobs. No -reset is used here, so the job numbers keep increasing.
#   but we unset the args,# variable after we access it's data (only to demonstrate how to do it)
#
#   After it starts up, each task will open a code window (likely on top of one another). Choose
#   to step or run either. it will resize and position one time. You can also play with the delay 
#   controls and watch the task_monitor.
#
#   to also instrument the fibonaci proc itself, enter in a command window:  eval [i fibonacci]
#   to stop instrumenting it enter:   i fibonacci -revert   (with or without the eval is ok)   
#

#####################################################################################
#   First, setup the paths below in the two places, and also the Tasks module as well.
#####################################################################################


    source "D:/stuff/vw debugging.tcl"      ;# we load the debugger 3 times, once here and in each task
    set ::___zz___(bp_messages_default)  0  ;# we can change the config parameters w/o modifying the file

    
    tcl::tm::path add d:/stuff              ;# set to path where to find tasks (say [pwd] if in local)
    package require tasks 1.13
    namespace import tasks::*
    
    
    catch {
        console show
        console eval {wm geom . 64x22+1100+404}
    }
    
    proc fibonacci {n} { ;# standard tcllib source code from the math library
        if { $n == 0 } {
            return 0
        } else {
            set prev0 0
            set prev1 1
            for {set i 1} {$i < $n} {incr i} {
                set tmp $prev1
                incr prev1 $prev0
                set prev0 $tmp
            }
            return $prev1
        }
    }

    Tproc fibsize {num} {   ;#our Tproc with 2 tasks plus trace callbacks when each job is done
        # need one breakpoint before the code window is created, then we can change it's title
        wait 300
        if { [incr ::once] == 1 } {                     ;# so we only do this once
            if { [string index $::t_name end] eq "0"} { ;# set the 2 code windows size/location - after hit run
                wm geom .lbp_console 747x535+652+390    ;# use the last char of the task name, a digit 0/1 for 2 tasks
            } else {
                wm geom .lbp_console 642x537+6+386  
            }
        }
        wm title .lbp_console "$::t_name job: $num"  ;# delay so we can see title, it will change back on next job
        wait 700
        set n [fibonacci $num]                       ;# do the real work here
        return "Size -> [string length $n]" 
    } -tasks -2 -import_tasks    [list  "# this is a comment list element"                                                          \
                                        {-set ::t_debug 0x2}                  "# direct all task puts/putz to console or stdout"    \
                                        {+debug=D:/stuff/vw debugging.tcl}    "# *** need to setup the correct path here ***"       \
                                        {#-if {$::t_name eq "fibsize0"} {eval [instrument+ fibsize] } }  "# use this to just instrument in one task"  \
                                        fibonacci                                                                                   \
                                        #+fibonacci                           "# import fibonacci"                                  \
                                        +fibsize                              "# import fibonaci and instrument fibsize"            \
                                                                                                                                    \
                                        {-package require Tk ; wm withdraw .}  "# we got enough windows already"                    \
                                 ]
                                
#   if -tasks is negative, we'll also have a trace callback, must define between the Tproc and the tgroup

    proc fibsize {aname element opcode} {                   ;# this is the trace callback
        set result  $::fibsize($element)                    ;# this is the result
        set job     [lindex [split $element ,] end ]        ;# get job number from rvar,job#
        set jobargs $::fibsize(args,$job)                   ;# the args used for this job
        unset ::fibsize(args,$job)                          ;# unset args
                puts "  .............. in trace, callback: $jobargs -> $result  "
    }
                                   
    task_monitor
    wm withdraw .

    proc queue_size {name} {
        return [llength [lindex [tdump +${name}0,queue\t] 0 1]] ;# how to use tdump to retreive the task #0's queue size
    }
    
    vw+ {fibsize  qsize}   "a Title for this viewer window"   ;# (must start with lower case) to view return value and queue size

    set max 15                                                          ;# can grow by this + 10
    while { 1 } {
        puts "\nstarting 10 jobs cur queue size= [queue_size fibsize]"  ;# report current queue size
        tgroup fibsize -foreach 1000 1200 1300 1400 1500 1600 1700 1800  1900 10000 ;#launch 10 jobs async
        update                                                          ;# to force out the puts message
        set once 0
        while { 1 } {
            set qsize [queue_size fibsize]                              ;# what's the size now
            if { $qsize > $max } {                                      ;# if it's too full now, wait
                if { [incr once] < 2} {
                    puts "waiting for queue size ($qsize ) to be less than $max"
                }
#               tgroup fibsize -wait all                                  ;# this would drain it down to 0 each time can deadlock though
                                                                        ;# if stopped in the debugger in say one of the tasks
                wait 1000                                               ;# alternatively, we might just wait 1 sec
                continue
            } else {
                puts "queue size: $qsize waiting 5 secs to start 10 more"
                wait 5000 ;# just hanging around
                break     ;# out of inner while loop, up to start off the new jobs
            }
        }
    }
    
    
