# Example 1: Demonstrate several Tasks run with the vw_debugger and task_monitor
#
#
#   To run: cd to where the release files were unzipped and run with wish or tclsh
#
#   In this example, we have the main thread sending jobs to the 2-task group: fibsize.
#   When each task starts up,  a debug code window is created, moved and resized and begin running
#
#   Also running is the task_monitor. One can pause both tasks or stop them individually in the debugger.
#   After each 10 jobs are completed, the results will be output and another 10 will be queued
#
#   Stopping either task in the debugger will cause the other to do all the work, but still must wait until the
#   last job in the paused task completes before queueing up another 10 jobs. Can step or run that task till it is done
#   with the job it was working on. That will complete the 10 jobs so another 10 can start up.
#
#   The example shows how to use comments in the initializer list of the Tproc and several other initializations.
#
#   The global variable that controls coverage tracks is also turned on. That can be turned off with the menu command checkbox.
#
#   Note: on linux, this example can sometimes crash Tk if using earlier than 8.6.12, before the Tk lock bug was fixed
#   On windows, it will not crash. The Tk linux bug was from starting Tk in multiple threads at the same time
#####################################################################################

set path_to_tasks [pwd]              ;# assume we are running in the unzip folder
set path_to_debug [file join $path_to_tasks vw_debugging.tcl]

tcl::tm::path add $path_to_tasks     ;# make this a known moudle path
package require tasks                ;# load the tasks module

package require Tk
namespace import tasks::*

source $path_to_debug

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

Tproc fibsize {num} {   ;#our Tproc with 2 tasks 
    # need one breakpoint before the code window is created, then we can change it's title
    wait 300
    if { [incr ::once] == 1 } {                     ;# so we only do this once
        if { ([string index $::t_name end] & 1 )== 1 } { ;# set the 2 code windows size/location
            wm geom .lbp_console 747x535+652+390    ;# use the last char of the task name, a digit 0/1 for 2 tasks
        } else {
            wm geom .lbp_console 642x537+6+386
        }
    }
    wm title .lbp_console "$::t_name job: $num"  ;# some info into the window title
    wait 700                                     ;# delay so we can see title, it will change back on next job
    set n [fibonacci $num]                       ;# do the real work here
    return [list $num [string length $n]]        ;# return a list of the number and its size
    
} -tasks 2 -import_tasks    [list                                                                     \
        {-set ::t_debug 0x2}                    "# direct all task puts/putz to console or stdout"    \
        "+debug=$path_to_debug"                 "# load debugger in each task"                        \
        {fibonacci}                             "# import fibonacci"                                  \
        +fibsize                                "# instrument fibsize"                                \
        {-after 2000 {go+ -999999}}             "# after 2 seconds start each task running"           \
        {-set ::___zz___(coverage)    1}        "# set coverage mode in code window"                  \
        {-if {$::t_name eq "fibsize1"} {after 50} } "# run staggered to lessen linux Tk crash chance" \
        {-package require Tk ; wm withdraw .}   "# we got enough windows already"                     \
    ]

task_monitor
wm withdraw .


vw+ {fibsize  qsize}   "a Title for this viewer window"   ;# title must start with lower case ??


while { 1 } {                                             ;# fire off 10 jobs, wait till done, then 10 more...
    incr group
    puts "\nstarting 10 jobs group $group"
    foreach job  [list 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900] {
        tgroup fibsize -foreach [expr {    $job + $group   }]       ;#launch the 10 jobs async
    }
    tgroup fibsize -wait all                                        ;# wait for all to complete before we queue 10 more
    foreach rvar [lsort -dictionary [array names fibsize rvar,*] ] {
        lassign  $::fibsize($rvar) num size
        puts "        $num -> $size  rvar= $rvar"
    }
    tgroup fibsize -reset
    puts "done group $group [string repeat \u2713 [expr {    $group % 10   }]]"
}
