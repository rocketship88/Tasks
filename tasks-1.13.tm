package require Thread
    
tsv::set  tids [thread::id] mainthread  ;# for reverse lookup 
tsv::set  main mainthread [thread::id]  ;# for reverse lookup 
################################################# Tasks version 1.13c
namespace eval tasks {  

#   This version provides the windows system with a puts wrapper. puts can have 1-3 arguments. 
#   
#   With 1 arg, stdout is assumed for the io channel and the puts wrapper will hand it off to putz 
#   to output. BTW, with 1 arg, the string can be -nonewline and is not an option, but the string.
#    
#   With 2 args, if the 1st arg is exactly -nonewline, then the second arg is the output string, 
#   and the io channel is again stdout. This too will be handed off to putz, which has been 
#   enhanced to also accept a -nonewline as it's first argument. 
#    
#   With 2 args and the first is not -nonewline, then the first arg is the io channel and the 
#   second is the string. In this case, the io channel can be stdout, stderr, or another channel. 
#   If stderr/stdout it will be handed off to putz. If some other value, it is assumed to be a 
#   true i/o channel (stdout/stderr are psuedo channels that use the console). This is then sent 
#   to the unwrapped and renamed puts for output to that channel.
#    
#   With 3 args, the first must be -nonewline (or it's an error) the 2nd is the i/o channel and 
#   3rd is the string. If it is -nonewline and the i/o channel is not stderr/stdout, it is also 
#   sent off to the unmodified puts. If it is stdout or stderr, then it is sent to putz. If 
#   stderr, then putz is sent the -nonewline, the string, and the color red.
#    
#   The debug variable t_debug controls putz output. When 0 or 1, output is to a tk window. If it 
#   sees the -nonewline and it's going to the tk window, it will suppress the newline. If tdebug 
#   is 2 or 3, then it is sent to the main thread and uses puts there to output to the console. 
#   There is no console in a thread. There isn't even a console command, and the channels stdout 
#   and stderr are not defined. This is why putz was created in the first place.
#    
#   Bottom line is one should be able to use puts in the normal fashion from a task thread.
#    
#   On linux, the output is handed off to puts in all cases. However, now the -nonewline should be 
#   passed on as well.
#    
#   The bug with linux using tk in more than one thread is supposed to be fixed; it mentions this 
#   in the 8.6.12 release and the code for the fix involves some locks. I've not tested this on linux, 
#   nor mac or androwish. For now, the code will still not create a tk window, but can be forced 
#   to do so by setting the t_debug to 4 or 5. Odd numbers also turn on a trace.

proc putz {args} { ;# debugging put using a text widget from a Task (a thread) args were: arg {color normal} {debug no}
#set ::t_puts_debug([incr ::puts_debug_num]) [string map {\n \u2936 \t \u02eb  { } \u2219} [string range "putz: $args" 0 50] ] 
    if { [lindex $args 0] eq "-nonewline" && [llength $args] > 1 } { ;# mimic behavior of puts
        set nonl 1
        lassign $args dummy arg color debug
    } else {
        set nonl 0
        lassign $args arg color debug
    }
    if { $color eq "" } {
        set color "normal"
    }
    if { $debug eq "" } {
        set debug "no"
    }
##########################################
#   t_debug -1 means no putz output at all
#   t_debug 0  means we use the tk, but no  debug output - the default if < we use 0
#   t_debug 1  means we use the tk  and yes debug stuff
#
#   t_debug 2  means we don't use the tk, and also no  debug
#   t_debug 3  means we don't use the tk, and also yes debug output debug
#
#   t_debug 4  overide protection and allow linux to use tk, 4 becomes a 0 - this is to save using another global this is getting ugly
#   t_debug 5  overide protection and allow linux to use tk, 5 becomes a 1
#
#   when the value is in hex, i.e. begins with a 0x, then the . tk window is withdrawn (now that we have our own toplevel) 
#
#   platform is windows     ok
#   platform is not windows then we add 2 to the value of t_debug if < 2 and not > 3
##########################################
#   set mid [tsv::get main mainthread]
#   thread::send -async $mid [list puts stderr "arg= |$arg| color= |$color| debug= |$debug| "]
#
    set dodebugging     0
    set dotk        0
    set overide     0
    if { ! [info exist ::t_pid] } { ;# check this, only exists in a task, not main (t_debug might be set by main, so can't use that)
        set io stdout
        if { $color ne "normal" && $::tcl_platform(platform) eq "windows"} {
            set io stderr
        }
        if { [info exist ::t_debug] && $::t_debug < 0 } {
            return
        }
        if { $nonl } {
            puts -nonewline $io $arg
        } else {
            puts $io $arg
        }
        return
    }
    set tdebug $::t_debug
    if       { $tdebug < 0 || $::t_putz_output == 0} {
        return
    } elseif { $tdebug > 3 } {
#        error "t_debug set to > 3 an invalid setting"
        set overide 1       ;# to use on linux systems anyway
        incr tdebug -4      ;# change to 0/1 for 4/5
    }
    if { $::tcl_platform(platform) ne "windows"  } { ;# hack: change windows to windowsx to force it to use stdout/stderr and a puts
        if { $tdebug < 2 && ! $overide } {
            incr tdebug 2                           ;# hack 2, if commented out, linux can also use tk windows, but beware, tcl/tk might abort with a seg fault or other crash
        }
    }
    
    if       { $tdebug == 0} {
        set  dotk 1
    } elseif { $tdebug  == 1} {
        set  dotk 1
        set  dodebugging 1
    } elseif { $tdebug  == 2} {
        # ok as is, both 0
    } elseif { $tdebug  == 3 } {
        set dodebugging 1
    } else {
        error "bad value for tdebug"
    }
#   tasks::tset $::t_name user "dotk= |$dotk| dodebugging= |$dodebugging| tdebug= |$tdebug| color= |$color| debug= |$debug| ::t_debug= |$::t_debug| "
#   error "dotk= |$dotk| dodebugging= |$dodebugging| tdebug= |$tdebug| color= |$color| debug= |$debug| ::t_debug= |$::t_debug| "
#   return  ;# to turn off debugging putz calls always
    if { $debug  eq "debug" && $dodebugging == 0} {
        return  
    }
    if { $dotk == 0 } {
        set mid [tsv::get main mainthread]
        set argg "[format %-10s  $::t_name] ! $arg"
        set io stdout
        if { $color ne "normal" && $::tcl_platform(platform) eq "windows"} {
            set io stderr
        }
#       tsv::set tvar $::t_name,user4 $mid

        if { $::tcl_platform(platform) eq "windows" } {
            if { $nonl } {
                thread::send -async $mid [list puts -nonewline $io $argg]
            } else {
                thread::send -async $mid [list puts $io $argg]
            }
        } else {
            if { $nonl } {
                puts -nonewline $io $argg
            } else {
                puts $io $argg  
            }
        }
        return
    }
    if { [info command .taskdebug.ttttt] eq "" } {                  ;# ![info exist ::t_putz] 
#        set ::t_putz 1
        if [catch {
            package require Tk
        } err_code] {
            tsv::set tvar $::t_name,errorTk "tdebug= |$tdebug| dotk= |$dotk| dodebugging= |$dodebugging| debug= |$debug| arg= |$arg| err_code= |$err_code| "
            return
        }
        if { [string range $::t_debug 0 1] eq "0x" } {
#           wait 15000
            catch {wm withdraw .} ;# now that we use our own toplevel, if this is inited to 0x0 we close this, user can set to 0 or other value to override
        }
        toplevel .taskdebug
        wm title .taskdebug "$::t_name putz"
        tsv::set tvar $::t_name,putz yes
        if [catch {
            set tname [tsv::get tids [thread::id]]
        } err_code] {
            set tname "No Task" 
        }
#        catch {wm title . $tname}
        frame  .taskdebug.fffff
        button .taskdebug.fffff.bbbbb -text "Exit [thread::id] $tname"   -command exit
        button .taskdebug.fffff.ccccc -text "Clear"                      -command {.taskdebug.ttttt delete 1.0 end}
        button .taskdebug.fffff.wwwww -text "Wider->"                    -command {wm geom .taskdebug [expr [lindex [split [wm geom .taskdebug] x] 0]+100]x[lindex [split [wm geom .taskdebug] x] 1]}
        
#       set ::t_task_pause 0 ;# don't set this here anymore, allows task to start up paused by setting this to 1
        checkbutton .taskdebug.fffff.cbcbcb1 -variable ::t_task_pause -text "pause"
        
        text      .taskdebug.ttttt    -yscrollcommand     {.taskdebug.sssss set}    -tabs {32 left} -tabstyle wordprocessor
        scrollbar .taskdebug.sssss    -command            {.taskdebug.ttttt yview}
        
        pack .taskdebug.fffff -side top -fill x
        pack .taskdebug.fffff.wwwww .taskdebug.fffff.ccccc -side left -expand 1 -fill x
        pack .taskdebug.sssss -side right -fill y
        pack .taskdebug.ttttt -side left -fill both -expand 1
        pack .taskdebug.fffff.cbcbcb1 -side left -fill y
        
        set ::t_putz_output 1
        checkbutton .taskdebug.fffff.cbcbcb2 -variable ::t_putz_output -text "putz output"
        pack .taskdebug.fffff.cbcbcb2 .taskdebug.fffff.bbbbb -side left -fill y
        set back grey30
        set fore white
        .taskdebug.ttttt tag configure debug              -foreground black -selectbackground $back -selectforeground $fore
        .taskdebug.ttttt tag configure normal             -foreground black -selectbackground $back -selectforeground $fore
        .taskdebug.ttttt tag configure green              -foreground \#408f40 -background \#e8e8e8 -font {courier 10 bold} -selectbackground $back -selectforeground $fore
        .taskdebug.ttttt tag configure white              -foreground white -background black  -font {courier 10 bold} -selectbackground $back -selectforeground $fore
        .taskdebug.ttttt tag configure yellowonblack      -foreground yellow -background black -font {courier 10 bold} -selectbackground $back -selectforeground $fore
        .taskdebug.ttttt tag configure yellow             -foreground yellow -background red -selectbackground blue
        .taskdebug.ttttt tag configure whiteonred         -foreground white -background red -font {courier 10 bold} -selectbackground black
        .taskdebug.ttttt tag configure rederror           -foreground red -background grey85 -font {courier 15 bold italic} -selectbackground black
        .taskdebug.ttttt tag configure red                -foreground red -font {courier 10} -selectbackground $back -selectforeground $fore
    }
    if [catch {
        if { $nonl } {
            .taskdebug.ttttt insert end $arg $color
        } else {
            .taskdebug.ttttt insert end $arg\n $color
        }
        .taskdebug.ttttt see end
        update
    } err_code] {
    }
}

proc wait { ms } {              ;# non busy wait
    set uniq [incr ::__sleep__tmp__counter]
    set ::__sleep__tmp__$uniq 0
    after $ms set ::__sleep__tmp__$uniq 1
    vwait ::__sleep__tmp__$uniq
    unset ::__sleep__tmp__$uniq
}
    
#################################################
    
proc xwait {arg {doupdate 1} {doputz 0}} {              ;# a busy wait version of wait, to test compute bound
    set max [expr {   $arg * 12000   }]
    for {set m 0} {$m < $max} {incr m} {
        incr mm
        if { ($m % 100000) == 0 } {
            if { $doupdate } {
                update
            }
        }
    }
    if { $doputz } {
        putz "xwait max= |$max|" normal debug
    }
}


#################################################

proc comma {num {sep ,}} { ;    ;# commify a positive number
    while {[regsub {^([-+]?\d+)(\d\d\d)} $num "\\1$sep\\2" num]} {}
    return $num
}

#proc Task package code         -----------------------------------------------------------
################################################# return a name from a tid
proc tname {tid} {              ;# shorthand to get the taskname given a Task id
    return [tsv::get tids $tid]
}
################################################# get or set by taskname and parm
proc tset {name parm {arg {GwY6itRvUgUNuTg2WfS3xyz123}}} {  ;# shorthand to get or set a shared variable given a Task name and element (optional value)
    set items [list tid pid result script mutex gvar cond queue count error share user putz paused]
    
    if { $arg != {GwY6itRvUgUNuTg2WfS3xyz123} } {
        foreach item $items {
            if { $parm eq $item } {
                return [tsv::set tvar $name,$item $arg]
            }   
        }
    } else {
        foreach item $items {
            if { $parm eq $item } {
                return [tsv::set tvar $name,$item]
            }   
        }
    }
}
#proc tget/tset alias           -----------------------------------------------------------
#interp alias {} tget {} tset
################################################# dump all shared variables
proc tdump {{pat .*} {max 90}} {         ;# dump all the shared Task variables
    set all 1
    set doputz 1
    set out {}
    if {       [string index $pat 0] eq "-" } { ;# a leading - reduces output to just the variables
        set all 0
        set pat [string range $pat 1 end]
    } elseif { [string index $pat 0] eq "+" } { ;# a leading + no output putz either AND return results in $out
        set all 0
        set doputz 0
        set pat [string range $pat 1 end]
    }
    if { $all } {
        putz "\n------ Task(s) dump -----------------------------------------"
        putz "tsv::names  = |[tsv::names *]|"
        putz "tsv::tids   = |[tsv::array names tids *]|"
        putz "---------------------------------------------------------------"
    }
    set tvarnames [lsort -stride 2 -index 1 [tsv::array get tids]]
    
    if { $all } {
        putz "tid/names   = |$tvarnames|"
        putz "---------------------------------------------------------------"
    }
    foreach {var val}  [lsort -dictionary -stride 2 -index 1 $tvarnames ] {
        if { $all } {
            putz "[format %-10s $val] tid: $var  exists: [thread::exists $var]"
        }
        
        set tidnames [tsv::array names tvar $val,*]
        foreach tname [lsort $tidnames] {
            set val [tsv::get tvar $tname]
            set val [string map {\n \u2936 \t \u02eb} $val]
            if { [regexp .*${pat}.* "$tname\t[string range $val 0 $max]"] } {
                if { $doputz } {
                    putz "                 [format %-27s ($tname)] = |[string range $val 0 $max]| "
                } else {
                    lappend out [list $tname $val]
                }
            }
        }
    }
    if { $all } {
        putz "---------------------------------------------------------------"
    }
    return $out ;# will be null unless +pat was used - to avoid dummping it all in interactive mode or windows console
}
#proc - main Task procs         -----------------------------------------------------------
#################################################
proc Task {name0 args} {        ;# create a Task
    set dowhile 1               ;# assume we want the automatic while loop, but if -once is any arg in args, we suppress it
    set donamespace 1           ;# assume we want to use namespaces, so we import by namespace
    set do_min_import 0
    while 1 {
        if {        [lindex $args 0] eq "-once" } {
            set dowhile 0
            set args [lrange $args 1 end]   ;# shift over the first item in args if -once is the next one
        } elseif {  [lindex $args 0] eq "-min_import_tasks"} {
            set do_min_import 1
            set args [lrange $args 1 end]   ;# shift over the first item in args if -min_import_tasks is the next one
        } elseif {  [lindex $args 0] eq "-import" || [lindex $args 0] eq "-import_tasks"} {
            set donamespace 0
            set args [lrange $args 1 end]   ;# shift over the first item in args if -import is the next one
        } else {
            break
        }
    }
    set len [llength $args]
    if       { $len == 0 || $len > 3 } {
        error "too few or too many args to Task = $len (or possibly a mispelled option)"
    } elseif { $len == 1 } {
        set args [list {} [lindex $args 0 ]]
    }
    set names [split $name0 /]
    if { [llength $names] == 1 } {
        set name $name0
        set share no
        set sname {}
    } elseif { [llength $names] == 2 } {
        lassign $names name sname           ;# my name plus which shared queue do we use
        set share yes
    } else {
        error "Invalid Task name |$name0|"
    }
#    if { [info exist ::t_debug] && $::t_debug } {
#        if [catch {
#        } err_code] {
#            catch {putz "Task: name= |$name| sname= |$sname| name0= |$name0| names= |$names| share= |$share| args(end-1)= |[lrange $args end-1 end-1]|"}
#        }
#    }
    
    if { [tsv::exists tvar $name,pid] } {
        error "Task \"$name\" already in use, only one task per taskname"
    }
    set me [thread::id]
    tsv::set tvar $name,pid $me             ;# save current parent pid
    tsv::set tvar $name,gvar {}             ;# used by tresult and tcall for a global to wait on
    tsv::set tvar $name,result {}           ;# the result
    tsv::set tvar $name,count 0             ;# the number of times waked up
    tsv::set tvar $name,error {}            ;# the last error if any
    tsv::set tvar $name,share {}            ;# the shared queue if any
    tsv::set tvar $name,putz {}             ;# set to yes if a putz called, for straighting windows
    tsv::set tvar $name,user {}             ;# an extra shared variable the user can use
    
    if { $share } {
        set mutex   [tsv::get   tvar    $sname,mutex]
        set cond    [tsv::get   tvar    $sname,cond]  
        tsv::set    tvar    $name,share     $sname   
        
    } else {
        set mutex   [thread::mutex create]  
        set cond    [thread::cond create]    
    }
    tsv::set    tvar    $name,queue     {}              ;# setup the cond/mutex and the queue   
    tsv::set    tvar    $name,mutex     $mutex  
    tsv::set    tvar    $name,cond      $cond   
    
    set dw1 "while 1 \{"        ;# also enclose our script in a while 1 loop unless the option -once is used
    set dw2 "\}"
    if { ! $dowhile } {
        set dw1 ""
        set dw2 ""
    }
    set e1 "if \[catch \{$dw1"  ;# enclose script in a catch, and a while (unless suppressed with the option)
    
    set e2a "\n\}$dw2 thread_err_code thread_err_dict\] \{\n    tsv::set tvar $name,error \$thread_err_dict  \n"
    set e2b {    package require Tk; tk_messageBox -message "Name = $::t_name Parent = $::t_pid\n$thread_err_code\n\n$thread_err_dict" -title "tid [thread::id]" ; vwait ::forever1}
    set e2c "\n\}\n"
    
    set e2 ""
    append e2 $e2a $e2b $e2c
    
    if { $do_min_import } {
        set autoimport [list ::tasks::treturn ::tasks::twait ]
    } else {
        set autoimport [list ::tasks::tlg ::tasks::tla ::tasks::twidgets ::tasks::tproc ::tasks::tdump ::tasks::putz ::tasks::treturn \
                        ::tasks::wait ::tasks::tset ::tasks::tcall ::tasks::twait ::tasks::Task ::tasks::tgroup \
                        ::tasks::xwait ::tasks::comma ::tasks::tname ::tasks::tvwait ::tasks::tpause_check ::tasks::Tproc ]
    }
    if { ! $donamespace } {
#        set autoimport [string map {::tasks {}} $autoimport]
        lappend autoimport {-namespace import tasks::* ;# from -import_tasks}
    }
        
        
    set putswrapper ""
    if { $::tcl_platform(platform) eq "windows"}  {
        
set puts_script {

rename puts t_old_puts_wrapped
proc puts {args} {
    set pargs $args
    set nlflag 0
#    set ::t_puts_debug([incr ::puts_debug_num]) [string map {\n \u2936 \t \u02eb  { } \u2219} [string range "puts: $args" 0 50] ]    
    if { [lindex $pargs 0] eq "-nonewline"  } {;# no abrev allowed for this puts option
        set pargs [lassign $pargs dummy]
        set nlflag 1
    }
    if       { [llength $pargs] == 2 } {;# puts chan data
        lassign $pargs ch data
        if       { $ch eq "stderr" } {
            if { $nlflag } {
                putz -nonewline $data red
            } else {
                putz $data red
            }
            return ""
        } elseif { $ch eq "stdout" } {
            if { $nlflag } {
                putz -nonewline $data normal
            } else {
                putz $data normal
            }
            return ""
        } else { ;# other channel so just pass it through
        }
    } elseif { [llength $pargs] == 1 } { ;# puts data
        if { $nlflag } {
            putz -nonewline [lindex $pargs 0] normal
        } else {
            putz [lindex $pargs 0] normal
        }
        return ""
    }
    if [catch {
        t_old_puts_wrapped {*}$args
    } err_code] {
        putz "t_old_puts_wrapped: $err_code"
    }
    return ""
}

} ;# end puts_script

        set putswrapper $puts_script ;# add this on windows, to support puts calls from a task, translated to putz calls
    } ;# end check for windows
    
    set preamble "#Preamble\n\nnamespace eval tasks {}\nset ::t_pid $me\nset ::t_name $name\nset ::t_putz_output 1\nset ::t_twait_timeout 50\nset ::t_task_pause 0\nset ::t_debug 0x0\nset ::t_debug_contents end\n${putswrapper}[tproc {*}$autoimport]\n"
    if       { [llength $args] == 2 } {
        lassign $args prefix script00
        append script0 $e1 $script00 $e2
        set prefix0 {}
        
        
        foreach prx $prefix {
            if { [string index $prx 0] eq "-" } { ;# dont put a -command in the importing comment, it could have newlines, just indicate it was seen
                append prefix0 " {-cmd} "
            } else {
                append prefix0 " " $prx " "
            }   
        }

        append script $preamble  
        append script "set ::___tlg___  \[info globals\] ;lappend ___tlg___  ___tlg___ tk_library tk_patchLevel tk_strictMotif tk_version\n"
        append script "\n#end preamble\n" "\n#included procs/cmds: import list: \{$prefix0\}\n\n" [tproc {*}$prefix] $script0
        
    } elseif { [llength $args] == 1 } {
        lassign $args script0
        append script $preamble  "\n#end preamble\n" "\n#included procs: none\n\n"  $script0 
    } else {
        error "Wrong number of args to task (or mispelled -option): $args"
    }

    set script0 "" ;# place another if/catch around the entire script, to catch things like namespace eval missing
    append script0 "if \[catch \{\n" $script "\n" "\} err_code_Task_Create\] \{ " "\n" "    tsv::set tvar $name,error \$err_code_Task_Create\n    package require Tk; tk_messageBox -title {Task create error} -message \$err_code_Task_Create\n    vwait ::forever2\n\}"  
    set script $script0
    set tid [thread::create $script]
    
    tsv::set  tvar $name,tid        $tid
    tsv::set  tvar $name,script     $script
    tsv::set  tids $tid             $name       ;# for reverse lookup
    if { $share } {
#   tdump
#   vwait ffff
    }
    uplevel #0 set $name $tid
    return $tid
}
proc tproc {args} {             ;# get procedure(s) and return results, internal use by [Task]
    set once_tasks 1
    set output {}
    foreach arg $args {
        if { [string index $arg 0] eq "-" } {
            append output [string range $arg 1 end] "\n"
        } else {
            set found 0
            
            set nq [namespace qualifiers ::$arg]
            set nqe [namespace exist ::$nq]
            if { $nq ne "" && $nqe } {
                if { ([string trim $nq :] ne "tasks") } {
                    append output "namespace eval $nq {namespace export *}\n" ;# we export everything, user can import if desired
                } else {
                    if { $once_tasks } {
                        append output "namespace eval $nq {namespace export *} ;# do this one time for tasks\n" ;# output this only once
                        set once_tasks 0
                    }
                }
            } else {
            }
            foreach proc [info procs ::$arg] {
                set found 1
                set space ""
                append output  "proc $proc {"
                    foreach arg [info args $proc] {
                        if [info default $proc $arg value] {
                            append output  "$space{$arg \{$value\}}"
                        } else {
                            append output  $space$arg
                        }
                        set space " "
                    }
#                   No newline needed because info body may return a
#                   value that starts with a newline
                    append output  "} {"
                    append output   [info body $proc]
                append output "}\n"
            }
            if { $found == 0 } {
                error "No imports found for $arg\n"
            }
        }
    }
    return $output
    # the below can fail, so no longer used, we just return the output as is
    set lines [split $output \n]
    set out {}
    foreach line $lines {
        if { [string index $line 0] eq "#" && [regexp {[\\{\\}]} $line] == 0 } { ;# don't import comment lines, just a blank comment line instead (so line numbers don't change)
            set line "#" ;# but if the line includes braces, it must be imported anyway
        }
        append out $line \n
    }
    return $out
}
proc treturn {args} {           ;# return the value from a Task
#    set exiting no
#    if { [llength $args ] > 0} {
#        if       { [lindex $args 0] eq "-exit"} {
#            set exiting yes
#            set args [lrange $args 1 end]
#        }
#    }
    if [catch {
        set rvalue $args
        set me      [thread::id]
        set name    [tsv::get   tids            $me]
        set pid     [tsv::get   tvar            $name,pid]
        tsv::set    tvar        $name,result    $args
        set gvar    [tsv::get   tvar            $name,gvar]
        
        if { $args == {} } {
            thread::send $pid [list set ::$gvar $rvalue]    ;# to allow for an empty return value
        } else {
            thread::send $pid "set ::$gvar $rvalue"         ;# to allow for a simple text string or a [list]
        }
        
    } err_code] {
        putz $err_code
    }
}

#################################################
proc tcall {taskid args} {      ;# call a Task, sync or asyn
    if { $taskid eq "-async" } { ;# allow -async to precede the taskid (thread id) for consistency with thread::send
        tailcall tcall [lindex $args 0] -async {*}[lrange $args 1 end]
    }
    if [catch {
        set exists [thread::exists $taskid] ;# this can return 0 or an error if id is not a thread id
    } err_code] {
        set exists 0
    }
    if {! $exists } {
        if [catch { ;# did the caller use the task name and not it's value?
            set tid $taskid
            set taskid [tsv::set tvar $taskid,tid] ;# try this instead
        } err_code] {
            putz $err_code 
            error "Thread '$taskid' does not exist"
        }
        error "Task id $tid does not exist, likely forgot to use \$$tid"
    }

    set name [tsv::get tids $taskid]
    set async no
    if { [llength $args ] > 0} {
        if       { [lindex $args 0] eq "-async"} {
            set async yes
            set args [lrange $args 1 end]
        }
    }
    set a1 [string range [lindex $args 1 ] 0 1]
    if { $a1 eq "<-" } {
        set args [lreplace $args 1 1]
    }
    if { [llength $args ] > 0} {
        set theglobal   [lindex $args 0]
        set args        [lrange $args 1 end]
    } else {
        error "tcall missing the argument for global variable"
    }
    if { [string range $theglobal 0 1] ne "::" } {
        set theglobal "::$theglobal"
    }
#   global $theglobal
    unset -nocomplain $theglobal
    
#   thread::send ?-async? ?-head? id script ?varname?
#   return
    
    set mutex   [tsv::set tvar $name,mutex]
    set cond    [tsv::set tvar $name,cond]
    set argsx   [list]
    lappend     argsx   [thread::id]    $theglobal $args
    
#   tsv::set tvar $name,gvar $theglobal ;################## the problem
    
    
    thread::mutex   lock    $mutex
    tsv::lpush      tvar    $name,queue $argsx end
    thread::cond    notify  $cond
    thread::mutex   unlock  $mutex
    
    if { $async } {
        if [catch {
        } err_code] {
            putz "async cannot use puts here $err_code" green debug
            catch {putz "send  $name  $taskid   args -async = |$args|"} ;# try again but to the thread instead
        }
        return 1
    } else {
        if { ![tvwait $theglobal $taskid] } {
            return {}
        }
        return [set $theglobal]
#       return [tsv::set tvar $name,result] ;# note, if we are using a shared queue, this will not be right, must use global var instead
    }
    
    
}
#################################################
proc tpause_check {args} {
    set twcount 0
    if { $::t_task_pause } {
        while { $::t_task_pause } {
            if { [incr twcount] == 1 } {
                putz "Pausing  task: $twcount"
                tsv::set tvar $::t_name,paused 1
            }
            wait 1000
        }
        putz "Resuming task after:  $twcount seconds"
        wait 1000
    }
    tsv::set tvar $::t_name,paused 0
    
}

proc twait {args} {             ;# wait for something in the Task queue
    if { $::t_debug < 0 } {
        set dbug 0
    } else {
        set dbug [expr {   $::t_debug % 2   }] ;# optimize the debug trace when not tracing -   t_debug is 0/2 no trace, 1/3 trace  
    }
#   wait 2000
    if { [info command tpause_check] ne ""} {
        tpause_check 
    }
    if [catch {
        set mutex [tsv::get tvar $::t_name,mutex]
        set cond  [tsv::get tvar $::t_name,cond]
    } err_code] {
        catch {putz $err_code}
    }
    if [catch {
        
#       wait 2000
        thread::mutex lock $mutex
#        set count 0
        set sname [tsv::get  tvar $::t_name,share]
        if { $sname != {} } {
            set tname $sname
        } else {
            set tname $::t_name
        }
        if { $dbug } {catch {putz "" normal debug}}
        if { $dbug } {catch {putz "sname(share name) = |$sname| tname(use)= |$tname| ::t_name(me)= |$::t_name| "  normal debug}}
        set count -1
        while {[tsv::llength tvar $tname,queue] == 0} {
            incr count
            if { $count < 1} { ;# output 1 times only each idle period
                if { $dbug } {catch {putz "queue is empty, so wait ($count)" red debug}}
            }
            thread::cond wait $cond $mutex $::t_twait_timeout
            update
        }
        if { $dbug } {catch {putz "queue not empty (retrys: [incr count]) len= [tsv::llength tvar $tname,queue] contents: [lrange [tsv::get tvar $tname,queue] 0 $::t_debug_contents ]" green debug}}
        set works [tsv::lpop tvar $tname,queue]
        thread::mutex unlock $mutex
        
        lassign $works pid gvar work ;# got these from the queue, now set gvar, so the return can use it, also the parent thread to return to
        tsv::set tvar   $::t_name,gvar  $gvar 
        tsv::set tvar   $::t_name,pid   $pid 
        if { $dbug } {
            set ms [clock milliseconds]
            set secs [expr {   $ms / 1000   }]
            set ms [string range $ms end-2 end]
            
            catch {putz "[string range [clock format $secs] 11 18].$ms job [expr {   [tsv::get tvar $::t_name,count]+1   }]: worklen= [llength $work] -> \{$work\} pid= $pid gvar= $gvar " yellowonblack debug}
        }
        
    } err_code err_dict] {
        set err [lrange [dict get $err_dict -errorcode] 0 1]
        tsv::set tvar $::t_name,error [list $err_code $err]
        if { $dbug } {catch {putz "error2: |$err_code| err |$err| " normal debug}}
        error  $err ;# propogate up, this could be a cancel, not sure what else to do here
    }
    tsv::incr tvar $::t_name,count

# should be able to do this with just one uplevel and lassign, but couldn't figure it out, so brute force it
    if { [llength $args] > 0 } { ;# if the call contained a variable to get the args, plus optionally variables to "lassign" them to
        set name [lindex $args 0]
        set rest [lrange $args 1 end]
        
        if { $dbug } {catch {putz  "args varname= |$name| rest of variables= |$rest|" normal debug}}
        uplevel set $name [list $work]
        
        set i -1
        foreach item $rest {
            incr i
            set data [lindex $work $i ] 
            if { $dbug } {catch {putz "     arg $i: [format %-12s $item ] data= |$data| " normal debug}}
            uplevel set $item [list $data]
        }
    
    }
    
    
    return $work
    
}
#################################################
proc tvwait {var {tid {}}} {    ;# wait till an async Task call, with Task id tid, completes and sets the variable
    if { [string range $var 0 1] ne "::" } {
        set var "::$var"
    }
    if { ![info exist $var] } {
        if {$tid != {} && ![thread::exists $tid] } { ;# if given a taskid, make sure it's still running or we wait forever
            set io stdout
            if { $::tcl_platform(platform) eq "windows" && 0} {
                set io stderr
            }
            if [catch {
                puts $io "Task: $tid does not exist, while waiting on $var"
            } err_code] {
                putz "error in twait: $err_code" normal debug
                catch {putz  "Task: $tid does not exist, while waiting on $var"}
            }
            return 0
        }
        vwait $var
    }
    return 1    
}
#################################################
    proc add_tasks {groupname  {add 1} {n 0}} {
#                                                                                                       global new_script_text
        if { $add == -9999 } {                                      ;# special, when called recursively
            upvar #0 $groupname name
            set nowthreads $name(threads)
            set script [tasks::tset ${groupname}0 script]           ;# we must have at least 1, so get this one's script
            set script_list [split $script \n]                      ;# split into lines
            set new_script {}
            set once 0
            foreach line $script_list {                             ;# go thru each line in the saved script
                lappend new_script $line                            ;# add this to the new script we are creating
                if { $line eq "set ::t_debug_contents end" && $once == 0} { ;# we insert some items after this statement
                    lappend new_script "set ::t_pid \[thread::id\]"
                    lappend new_script {set ::t_task_pause 1}
                    lappend new_script [tasks::tproc tasks::putz]
                    lappend new_script "set ::t_name ${groupname}$n" ;# set this again after we start him
                    incr once
                }
                if { $line eq "#end preamble" && $once == 1} {      ;# after we find this, we need to insert some more
# need to delay task startup until we can fully setup this task
#
                    lappend new_script "
                        while {\$::t_task_pause != 0 } {
                            after 100 {set ::t_name 1}
                            vwait ::t_name
                            incr ::t_times_we_waited
                        }
                        set ::t_name ${groupname}$n
                    "
                }
            }
            foreach tsvname [tsv::array names tvar ${groupname}0,*] {           ;# create his tsv nameN,field variables
                set tsvname_field [lindex [split $tsvname ,] 1 ]
                set value [tsv::get tvar ${groupname}0,$tsvname_field]
                if { $tsvname_field in {queue gvar putz result tid user script} } {     ;# these we init to null
                    set value ""
                }
                if { $tsvname_field in {count} } {                              ;# init to 0
                    set value "0"
                }
                tsv::set tvar ${groupname}$n,$tsvname_field $value              ;# create and init with $value
            }
            tsv::set tvar ${groupname}$n,share ${groupname}0                    ;# set the share value to the main task in the group
            incr name(threads)                                                  ;# this is the group array we upvar to
            incr name(tasks)                                                    ;# both these need to be incremented when we add 1 task
            set new_script_text [join $new_script \n]
            tsv::set tvar ${groupname}$n,script $new_script_text
            set newtid [thread::create $new_script_text]                        ;# now create the actual thread using our new script
            uplevel #0 set ::${groupname}$n $newtid                             ;# add a global/namespace var for the new tid
            set name(tid,$n) $newtid                                            ;# add to the output array tids
            tsv::lappend tids  $newtid ${groupname}$n                           ;# also add the tid vs. taskname to the tids shared array
            tsv::set tvar ${groupname}$n,tid $newtid                            ;# now set our tsv tvar name,tid for us, now that we know the tid
            thread::send -async $newtid {set ::t_task_pause 0}                  ;# we've been in a waiting loop, this resumes our thread now that it's ready to get work
            return $newtid
        }
        
# here is when we are NOT called recursively

#       tasks::tgroup $groupname -reset
        set nthreads [set ::${groupname}(threads)]
#                                                                                                       vwait ffff
        for {set n $nthreads} {$n < ($nthreads + $add )} {incr n} {
            add_tasks $groupname -9999 $n
        }
    } ;# end add_tasks
    
############ tgroup ##########################################################
#   
#   Multiple task builder. This takes gname and uses it for a
#   group of tasks. The names will be gname0, gname1, .... gname(N-1)
#   where gname0 is the boss and the rest will be helper/boss with
#   names like gname1/gname0, gname2/gname0, ... gname(N-1)/gname0
#   
#   If the number of tasks is negative, e.g. -4, then the abs(number) will
#   be used, but the -N also will then create traces on each of the tasks
#   assigned result variables (rvar,n). in the -call code, after the tcall's
#   
#
#   To create the tasks, one uses the -tasks N or -N option
#   
#       tgroup groupname -tasks N ....  args in Task starting with options
#       tgroup groupname -tasks -N .... this one creates a trace
#       
#   
#   
#   There are 2 ways to process, using -call or -foreach
#   
#       tgroup groupname -call args...
#       tgroup groupname -foreach args...
#       
#       -call
#    
#           can only have as many items as tasks, and if items are less
#           than the number of tasks, it will recycle from the beginning of
#           the arglist until it has run exactly N jobs for -tasks N
#           it is an error to have more args than tasks
#           
#       -foreach
#   
#           This can take any number of args 1..M and each will be run
#           regardless of how many tasks are created. if more jobs than
#           tasks, some tasks will run more than one job in sequence. This
#           option can be used more than once.
#
#   A trace, using -tasks -N can be applied to either -call or -foreach
#
#   To wait for these to be done
#       
#       tgroup groupname -wait all
#       
#           This will wait on all the tasks to complete.
#           
#   The groupname is also the name of a global array that will be
#   first unset on the -tasks option, and then the following elements are
#   generated:
#   
#   where n is the number 0..N-1 for N jobs
#   
#   rvar,n     the result value element the calls use
#   args,n     the arguments passed to the n'th task
#   tid,n      the task id's
#   
#   tasks      one only, the number of tasks or jobs depending on -call or -foreach
#   
#   The tasks are all linked to the first one, gname0, and they may exit, but
#   gname0 should not exit. If the script is the same for each, they can tell
#   if they are the boss task by testing for the number in ::t_name which is
#   always set to the task's name and will have a number at the end.
#   
#   usage:
#   
#   tgroup groupname -tasks ?-?N .... args to a task beginning with the options
#
#   tgroup groupname -call    {arglist 1} {arglist 2} ... (only 1 of these)
#   tgroup groupname -foreach {arglist 1} {arglist 2} ... (can be repeated)
#
#   tgroup groupname -wait all 
#   
# or to comine -foreach and -wait all to reduce to just 2 calls:
#
#   tgroup groupname -tasks ?-?N .... args to a task beginning with the options
#   tgroup groupname -run  {arglist 1} {arglist 2} ...    (only 1 of these)
#   tgroup groupname -reset                               (no args, reset counts to same as just after -tasks) 
############
    
proc tgroup {gname option args} {
    if       { $option eq "-tasks" } {
        uplevel #0 array unset $gname
        upvar #0 $gname name
        
        set argss [lassign $args num]
        if [catch {
            set name(trace) 0 ;# if we catch an error here, we likely already have a variable of the same name as the group, no can do
        } err_code] {
            if { [info exists name] } {
                error "error: $err_code\ngroup name $gname is already in use as a variable"
            } else {
                error "error: $err_code"    
            }
        }
        set name(job)   0  ;# so we can have multiple -foreach's (only 1 -call however)
        if { $num < 0 } {
            set num [expr {   0 - $num   }]
            set name(trace) 1
        }
        set name(tasks) $num
        set name(threads) $num
        for {set n 0} {$n < $num } {incr n} {
            if { $n == 0 } {
                set t ${gname}0
            } else {
                set t   ${gname}${n}/${gname}0
            }   
            set tid [uplevel [list tasks::Task $t {*}$argss]]
            set name(tid,$n) $tid
        }
        
    } elseif {  $option eq "-add_tasks" } {
        return [add_tasks $gname {*}$args]
    } elseif {  $option eq "-run" || $option eq "-reset" } {
        upvar #0 $gname name
        set name(job)   0                                              ;# reset this so we can do another -run
        set undef {}
        lappend undef  {*}[array names name rvar,*] {*}[array names name args,*]
        foreach und $undef {
            unset name($und)    
        }
        set name(tasks) $name(threads)
        if { [llength $args] != 0 && $option eq "-run"} { ;# no args after -reset
            tgroup $gname -foreach {*}$args
            tgroup $gname -wait all
        }
    
        
    } elseif {  $option eq "-foreach"} { ;# it's ugly but we allow multiple -foreach's in separate tgroup calls, so we must accumulate jobs
        upvar #0 $gname name
        set numtasks $name(tasks)
        set numarglists [llength $args]
        set name(tasks) [expr {   $numarglists + $name(job)   }] ;# this is ugly, we change meaning of tasks to jobs, since -wait will still work on number of tasks
        if { $name(job) < 0} {
            error "Cannot mix -foreach and -call current job =  $name(job)"
        }
        set jj -1
        for {set job $name(job)} {$job < $name(tasks) } {incr job} {
            set theargs [lindex $args [incr jj] ]
            set name(args,$job) $theargs
            set tid [tset  ${gname}0 tid]
            set tn  [tname $tid]
            set c [uplevel [list tasks::tcall $tid -async ::${gname}(rvar,$job) {*}$theargs]]
            if { $name(trace) } {
                if {! [info exist  ::${gname}(rvar,$job)] } {
                    trace add variable ::${gname}(rvar,$job) write $gname
                } else {
                    $gname  ::$gname rvar,$job Write
                }
            }
            
            if { $c != 1 } {
                error "error calling tgroup -call on job $job"
            }
            
        }
        set name(job) $job

    } elseif {  $option eq "-call"  } { ;# only one -call allowed
        upvar #0 $gname name
        if { $name(job) != 0 } {
            error "Cannot mix -foreach and -call or more than one -call: current job =  $name(job)"
        }
        set name(job) -1   ;# in case we try to do this again
        set numtasks $name(tasks)
        set numarglists [llength $args]
        if { $numarglists >  $numtasks} {
            error "tgroup $gname : too many arglists, $numarglists with only $numtasks tasks"
        }
        set index 0
        for {set job 0} {$job < $numtasks } {incr job} {
            if { ($job % $numarglists) == 0} {
                set index 0
            }
            set theargs [lindex $args $index ]
            set name(args,$job) $theargs
            set tid [tset  ${gname}0 tid]
            set tn  [tname $tid]
            set c [uplevel [list tasks::tcall $tid -async ::${gname}(rvar,$job) {*}$theargs]]
            if { $name(trace) } {
                if {! [info exist  ::${gname}(rvar,$job)] } {
                    trace add variable ::${gname}(rvar,$job) write $gname
                } else {
                    $gname  ::$gname rvar,$job Write
                }
            }
            
            if { $c != 1 } {
                error "error calling tgroup -call on job $job"
            }
            
            incr index
        }
        
    } elseif {  $option eq "-wait" } {
        upvar #0 $gname name
        lassign $args type
        if       { $type eq "all" } {
            set numtasks $name(tasks)
            for {set job 0} {$job < $numtasks } {incr job} {
                tvwait ::${gname}(rvar,$job)
            }
        } elseif { $type eq "one"  } {
            error "not implemented in tgroup yet $type"
        } else {
            error "Invalid tgroup call -wait $type must be all or one"
        }
    } else {
        error "Invalid option to tgroup: $option, must be -tasks, -call, -foreach, -run, -reset, -add_tasks or -wait"
    }
}
#   Notes on Tproc. 
#   The user can specify a -num for -tasks, just like in tgroup, however, this
#   means that the traceback proc will have to be after Tproc is called to overide 
#   the proc name, since tracebacks also use the task name. We have to create the proc using
#   the same name, so we can import it. Maybe we should do ${name}_orig for the proc name
#   
#   The user can also do -import or -import_tasks, and also -once, but if -once is used, then
#   the proc will exit after 1 call.
#
#   Note: Tproc uses tgroup on name, so name0,name1,...,nameN are generated, so cannot be used
#   for other Tproc's or tgroups as the name argument, or an error will result 

proc Tproc {name arguments body {option -tasks} {num 4} args} {
    uplevel [list proc $name $arguments $body]
    if { $option ne "-tasks" } {
        error "Tproc option $option invalid, should be -tasks"
    }
    set qual "tasks::"
    set opts {}
    set ar $args
    while { 1 } {
        set ar [lassign $ar option]
        if { $option eq "-import_tasks"  || $option eq "-import"} {
            lappend opts "-import_tasks"
            set qual ""
        } elseif { $option eq "-once" } {
            lappend opts "-once"
        } elseif { $option eq "-min_import_tasks" } {
            lappend opts "-min_import_tasks"
        } else {
            set ar [list $name {*}$option]
            break
        }
    }
    set targs {}
    lappend targs {*}$opts $ar
    uplevel "tasks::tgroup $name -tasks $num $targs  \{
        ${qual}twait argv ; set argc \[llength \$argv\]
        ${qual}treturn \[$name \{*\}\$argv\]
    \}
    "
}


#proc repos {{geomx 700x200} {rowsx 4} {xoffx 700} {yoffx 240} } ;# old way, now we compute, so need only enter 111x222, but can still specify others
proc repos {args} {
    lassign $args geomx  rowsx  xoffx  yoffx
    if { $geomx eq "" } {
        set geomx 700x200
    }
    lassign [split $geomx x] xsize ysize
    if { ![string is integer -strict $xsize] || ![string is integer -strict $ysize]  } {
        error "invalid XxY (xsize by ysize) usage: repos ?XxY? ?rows? ?x-offset? ?y-offset?"
    }
    
    set xsize [expr {   max($xsize,150)   }] ;# need minimum of 150
    set ysize [expr {   max($ysize,100)   }]
    set geomx ${xsize}x${ysize}              ;# reconstruct
    
    if { $rowsx eq "" } {
        set rowsx   [expr {   int( 1000 / ($ysize + 40))   }]
    }
    if { $xoffx eq "" } {
        set xoffx $xsize
    }
    if { $yoffx eq "" } {
        set yoffx [expr {   $ysize + 40   }]
    }
    
    
    set task -1
    foreach i [tasks::tdump {+,tid}] {
        lassign $i name tid
        if { $name eq "_taskmonitor,tid" || $name eq "sendcmd,tid" } {
            continue
        }
        set tname [lindex [split $name ,] 0]
        set count [tasks::tset $tname count]
        if { $count <= 0 } {
            #continue ;# if you only want tasks that have done something to be adjusted, enable this
        }
        if { [tsv::set tvar $tname,putz]  ne "yes"} {
            continue    ;# if this task  has no putz debug window, skip so task count doesn't increment and windows
        }
        incr task
        set script "set __repos__(num) $task\nset  __repos__(rows) $rowsx;set  __repos__(xoff) $xoffx; set  __repos__(yoff) $yoffx; set  __repos__(geom) $geomx\n"
        append script {
            
            set  __repos__(x) [expr {   ($__repos__(num) / $__repos__(rows))   * $__repos__(xoff)   }]
            set  __repos__(y) [expr {   ($__repos__(num) % $__repos__(rows))   * $__repos__(yoff)   }]
            set  __repos__(newgeom) "$__repos__(geom)+$__repos__(x)+$__repos__(y)"
            if [catch {
                wm withdraw .taskdebug
                wm deiconify .taskdebug
                wm geom .taskdebug $__repos__(newgeom)
                set __repos__(zzz) $__repos__(newgeom)
            } __repos__(err_code)] {
                set __repos__(zzz) "err=[set __repos__(err_code)]" ;# debugging, if it fails we can check this global in the thread
            }
            #set __repos__(zzz) "$__repos__(zzz) ---- $__repos__(num)   x= $__repos__(x)    y= $__repos__(y)" ;# for debugging this
        }
        after [expr {   100*$task   }] [list thread::send  -async $tid $script]
    }
} ;# end repos


###################################################################

proc task_monitor {args} {
    
#   --------------------------------------- create frames
    set create_frame_script {
    
           ;  proc save_layout {} {
                set io [open [file join [pwd] .taskmonitor_layout] w]
                puts $io [get_layout]
                close $io
                putz "Save layout to: [file join [pwd] .taskmonitor_layout]"
            }
           ;  proc restore_layout {} {
                set io [open [file join [pwd] .taskmonitor_layout] r]
                set data [split [read -nonewline $io] \n]
                set_layout {*}$data
                close $io
                putz "Restore layout from: [file join [pwd] .taskmonitor_layout]"
            }
    
        ; proc get_layout {} {
            set wids {}
            for {set n 0} {$n <= $::ncols } {incr n} {
                set widgetx $::widget(0,$n)
                set current [$widgetx cget -width]  
                lappend wids $current
            }
            return [list $wids [wm geom .top] $::refresh_seconds $::fsize $::reposX $::reposY $::changes]
        }
        ; proc set_layout {arg} {
            lassign $arg wids geom ref fnt x y chg
            if { $geom ne "" } {
                wm geom .top $geom
            }
            if { $ref ne "" } {
                set ::refresh_seconds $ref
            }
            if { $fnt ne "" } {
                set ::fsize $fnt
                font_callback
            }
            if { $x ne "" } {
                set ::reposX $x
            }
            if { $y ne "" } {
                set ::reposY $y
            }
            if { $chg ne "" } {
                set ::changes $chg
            }
            set n -1
            foreach wid $wids {
                incr n
                column_resize $n $wid   
            }
        }
        ; proc column_resize {col size} {
            set widgetx $::widget(0,$col)
            set current [$widgetx cget -width]
#           tasks::putz  "col= |$col| nrows= |$::nrows| which= |$which| widgetx= |$widgetx| current= |$current| " green
            for {set n 0} {$n <= $::nrows  } {incr n} {
#               tasks::putz  "   n = $n  widget($n,$col) = $::widget($n,$col)" red
#               tasks::putz "newcur= |$newcur| " 
                $::widget($n,$col) configure -width $size
                
            }
            event generate .top.fra.scframe.canvas <Configure> ;# should be able to compute this widget path, but...
        }
        ; proc column_sizer {mult col which widgetx} { ;# left/right clicks on heading will resize column left=smaller (min size 3) (max size 100)
            set current [$widgetx cget -width]
#           tasks::putz  "col= |$col| nrows= |$::nrows| which= |$which| widgetx= |$widgetx| current= |$current| " green
            for {set n 0} {$n <= $::nrows  } {incr n} {
#               tasks::putz  "   n = $n  widget($n,$col) = $::widget($n,$col)" red
                if { $which == 1 } {
                    set newcur  [expr {   max(1,$current - (3*$mult))   }]
                } else {
                    set newcur  [expr {   min(100,$current + (3*$mult))    }]
                }
#               tasks::putz "newcur= |$newcur| " 
                $::widget($n,$col) configure -width $newcur
                
            }
            event generate .top.fra.scframe.canvas <Configure> ;# should be able to compute this widget path, but...
            
        }
        
        ; proc add_row {newrow} {
            global nrows ncols widget table path
#           tasks::putz "nrows= |$nrows| ncols= |$ncols| newrow= |$newrow| "
            set last [expr {   $newrow - 1   }]
            set widgets [list]
            foreach col [range 0 ..  $ncols]     {
                set w0 $widget($last,$col)
                set wid [$w0 cget -width]
                set font [$w0 cget -font]
                set parent [winfo parent $w0]
                set newpath "$parent.path$newrow-$col"
#               tasks::putz "\ncol = $col last = $last   w0= |$w0| path= $path "    red
#               tasks::putz "newpath = |$newpath| width = $wid  font = $font"
                set w1 [entry $newpath      -textvariable ::table($newrow,$col) -width $wid -font $font -readonlybackground {} -state normal]
                if { $col == 0 } {
                    $w1 configure -bg grey80  -bd 5 -relief flat
                } else {
                    $w1 configure  -justify left -bd 5 -relief groove
                }
                set widget($newrow,$col) $w1
                set table($newrow,$col) {} 
                lappend widgets $w1     

            }
#           tasks::putz [join $widgets \n] yellowonblack
            grid  configure {*}$widgets
            incr nrows
        }
        
        ; proc create_table {top frame nrow ncol widths headers {fsize 12}} {
            global widget table
#           frame .frame
            foreach row [range 0 .. $nrow ] {
                set widgets {}
                foreach col [range 0 ..  $ncol ] {
                    set wid [lindex $widths $col]
                    if { $wid eq "" } {
                        set wid 15
                    }
                    set w [entry $frame.path$row-$col   -textvariable ::table($row,$col) -width $wid \
                            -font [list TkTextFont $fsize] -readonlybackground {} -state normal]
                    set widget($row,$col) $w
                    if { $row == 0 } {
                        bind $w <Button-1>  "column_sizer 1 $col %b %W ; break"
                        bind $w <Button-3>  "column_sizer 1 $col %b %W ; break"
                        bind $w <Shift-Button-1>  "column_sizer 5 $col %b %W ; break"
                        bind $w <Shift-Button-3>  "column_sizer 5 $col %b %W ; break"
                    }
                    if { $row == 0 } {
                        set table($row,$col) "[lindex $headers $col]"
                        $w configure -bg grey90 -justify center -bd 5 -relief raised
                        
                    } else {
                        $w configure  -justify left -bd 5 -relief groove
                        set table($row,$col) {} ;#"label $row x $col [string repeat 12345- 5]"
                        
                    }
                    lappend widgets $w
                }
                grid  configure {*}$widgets
            }
            foreach row [range 1 .. $nrow ] {
                $widget($row,0) configure -bg grey80  -bd 5 -relief flat
            }
            grid $frame
            event generate .top.fra.scframe.canvas <Configure>          
        }
        ; proc destroy_table {args} {
            global widget table
            foreach wida [array names widget] {
                destroy $::widget($wida)
                unset ::widget($wida)
            }
            destroy .frame
            
            
        }
        ; proc create_frame {top args} {
#           ----------------------------- wow, scrollable frame setup --------------
            if { [info command $top] == "" } {
                set top [toplevel $top]
            }
            set fra [frame $top.fra]
            set frascr $fra.scframe
            
            sframe new $frascr -mode xY -toplevel false  -anchor w
            
            set path [sframe content $frascr]
            
            pack $fra -expand 1 -fill both
            pack $frascr -expand 1 -fill both
            
            $fra.scframe.canvas configure -xscrollincrement 1
            $fra.scframe.canvas configure -yscrollincrement 1
            return $path
#           ------------------------------------------------------------------------
        }
        
    } ;# end create_frame_script
    
#proc end   create frames
    
#   --------------------------------------------- scrollable frame from wiki
    
    set sframe_script {
        
        namespace eval ::sframe {
            namespace ensemble create
            namespace export *
            
            # Create a scrollable frame or window.
            ;proc new {path args} {
                # Use the ttk theme's background for the canvas and toplevel
                set bg [ttk::style lookup TFrame -background]
                if { [ttk::style theme use] eq "aqua" } {
                    # Use a specific color on the aqua theme as 'ttk::style lookup' is not accurate.
                    set bg "#e9e9e9"
                }
                
                # Create the main frame or toplevel.
                if { [dict exists $args -toplevel]  &&  [dict get $args -toplevel] } {
                    toplevel $path  -bg $bg
                } else {
                    ttk::frame $path
                }
                
                # Create a scrollable canvas with scrollbars which will always be the same size as the main frame.
                set mode both
                if { [dict exists $args -mode] } {
                    set mode [dict get $args -mode]
                }
                switch -- [string tolower $mode] {
                    both - xy - yx {
                        set canvas [canvas $path.canvas -bg $bg -bd 0 -highlightthickness 0 -yscrollcommand [list $path.scrolly set] -xscrollcommand [list $path.scrollx set]]
                        ttk::scrollbar $path.scrolly -orient vertical   -command [list $canvas yview]
                        ttk::scrollbar $path.scrollx -orient horizontal -command [list $canvas xview]
                    }
                    y {
                        set canvas [canvas $path.canvas -bg $bg -bd 0 -highlightthickness 0 -yscrollcommand [list $path.scrolly set]]
                        ttk::scrollbar $path.scrolly -orient vertical   -command [list $canvas yview]
                    }
                    x {
                        set canvas [canvas $path.canvas -bg $bg -bd 0 -highlightthickness 0 -xscrollcommand [list $path.scrollx set]]
                        ttk::scrollbar $path.scrollx -orient horizontal -command [list $canvas xview]
                    }
                    default {
                        return -code error "-mode option is invalid: \"$mode\" (valid are x, y, xy, yx, both)"
                    }
                }
                
                # Create a container frame which will always be the same size as the canvas or content, whichever is greater.
                # This allows the child content frame to be properly packed and also is a surefire way to use the proper ttk background.
                set container [ttk::frame $canvas.container]
                pack propagate $container 0
                
                # Create the content frame. Its size will be determined by its contents. This is useful for determining if the
                # scrollbars need to be shown.
                set content [ttk::frame $container.content]
                
                # Pack the content frame and place the container as a canvas item.
                set anchor "n"
                if { [dict exists $args -anchor] } {
                    set anchor [dict get $args -anchor]
                }
                pack $content -anchor $anchor
                $canvas create window 0 0 -window $container -anchor nw
                
                # Grid the scrollable canvas sans scrollbars within the main frame.
                grid $canvas   -row 0 -column 0 -sticky nsew
                grid rowconfigure    $path 0 -weight 1
                grid columnconfigure $path 0 -weight 1
                
                # Make adjustments when the sframe is resized or the contents change size.
                bind $path.canvas <Configure> [list [namespace current]::resize $path]
                
                # Mousewheel bindings for scrolling.
                bind [winfo toplevel $path] <MouseWheel>       [list +[namespace current] scroll $path yview %W %D]
                bind [winfo toplevel $path] <Shift-MouseWheel> [list +[namespace current] scroll $path xview %W %D]
                bind [winfo toplevel $path] <Button-4>       [list +[namespace current] scroll $path yview %W 150]
                bind [winfo toplevel $path] <Button-5>       [list +[namespace current] scroll $path yview %W -150]
                bind [winfo toplevel $path] <Shift-Button-4>       [list +[namespace current] scroll $path xview %W 150]
                bind [winfo toplevel $path] <Shift-Button-5>       [list +[namespace current] scroll $path xview %W -150]
                
                return $path
            }
            
            
            # Given the toplevel path of an sframe widget, return the path of the child frame suitable for content.
            ;proc content {path} {
                return $path.canvas.container.content
            }
            
            
            # Make adjustments when the the sframe is resized or the contents change size.
            ;proc resize {path} {
                set canvas    $path.canvas
                set container $canvas.container
                set content   $container.content
                
                # Set the size of the container. At a minimum use the same width & height as the canvas.
                set width  [winfo width $canvas]
                set height [winfo height $canvas]
                
                # If the requested width or height of the content frame is greater then use that width or height.
                if { [winfo reqwidth $content] > $width } {
                    set width [winfo reqwidth $content]
                }
                if { [winfo reqheight $content] > $height } {
                    set height [winfo reqheight $content]
                }
                $container configure  -width $width  -height $height
                
                # Configure the canvas's scroll region to match the height and width of the container.
                $canvas configure -scrollregion [list 0 0 $width $height]
                
                # Show or hide the scrollbars as necessary.
                # Horizontal scrolling.
                if {[winfo exists $path.scrollx]} {
                    if { [winfo reqwidth $content] > [winfo width $canvas] } {
                        grid $path.scrollx  -row 1 -column 0 -sticky ew
                    } else {
                        grid forget $path.scrollx
                    }
                }
                # Vertical scrolling.
                if {[winfo exists $path.scrolly]} {
                    if { [winfo reqheight $content] > [winfo height $canvas] } {
                        grid $path.scrolly  -row 0 -column 1 -sticky ns
                    } else {
                        grid forget $path.scrolly
                    }
                }
                return
            }
            
            
            # Handle mousewheel scrolling.
            ;proc scroll {path view W D} {
                if { [winfo exists $path.canvas]  &&  [string match $path.canvas* $W] } {
                    $path.canvas $view scroll [expr {-$D}] units
                }
                return
            }
        } ;# end namespace sframe
        
    } ;# end sframe_script
    
#proc end sframe
    
    
set utility_scripts {
        
    ;proc range {a op b {by by} {step 1}} {
        if       { $op eq ".." || $op eq "to"} {
            if { $a > $b && $step > 0 } {
                set step [expr {   0 - $step   }]
            }
            if { $step == 0 || ($step < 0 && $a <= $b) || ($step > 0 && $b < $a)} {
                error "range: invalid step = $step with a = $a and b = $b"
            }
            if { $by ne "by" } {
                error "range: unknown term for by : $by"
            }
            set step [expr {   abs($step)   }]
            if { $a <=  $b } {
                incr a [expr {   0-$step   }]
                lmap b [lrepeat [expr {   ($b-$a) / $step   }] 0] {incr a $step}
            } else {
                incr a $step
                lmap b [lrepeat [expr {   ($a-$b) / $step   }] 0] {incr a -$step}
            }
        } elseif { $op eq "#" } {
            incr a [expr {   0-$step   }]
            lmap b [lrepeat $b 0] {incr a $step}
        } else {
            error "unknown range op $op"
        }
        
    }
    
#####################      send command task        ###########################
    
        
        
        
        
        
        
} ;# end utility_scripts
    
#proc end utility_scripts
    
    
#proc  task defintion
#   namespace import tasks 
    Task _taskmonitor  -import_tasks [list -$sframe_script -$create_frame_script -$utility_scripts tasks::repos tasks::* balloon::* {- #set t_debug 0} ] {
        set repos_rows ""
        set putzwindow ""

        tasks::twait -> init_nrows fsize repos_rows putzwindow    ;# max rows, font size to use, and if present monitor size vertical for repos, and putz window will open, 
        if { $init_nrows eq "" } {
            set init_nrows 10                   ;# default rows
        }
        tasks::treturn "initial rows: $init_nrows $repos_rows $putzwindow"
        
        if { $fsize eq "" } {
            set fsize 10                        ;# default font size if caller didn't supply one (missing args are always null)
        }
        if { $putzwindow ne ""  && $putzwindow} {
            tasks::putz "Initial rows: |$init_nrows|  Font Size = |$fsize| repos rows = |$repos_rows| putzwindow = |$putzwindow|"
        }
#       -------------------------------- gui ---------------------------------------------------------
        package require Tk
        wm withdraw .
        
        toplevel .top
        wm title .top  "TaskMonitor"
        set refresh_seconds 2                   ;# this also determines how long a changed value stays pink, user can change with spinbox
        set pause           0                   ;# we pause by just continuing in the main loop
        set changes         1                   ;# indicate changes with color
        set reposX          825
        set reposY          400
        set ontop           0
        
        ttk::labelframe .top.frame              -text "  Task Monitor Controls  "   -padding [list 5 2 5 2]
        
        ttk::labelframe .top.frame.delay        -text "Refresh  / Font"
        ttk::spinbox    .top.frame.delay.sb     -from .5  -to 5    -increment .5    -textvariable refresh_seconds   -width 4   -font {TkTextFont 14}
        ttk::spinbox    .top.frame.delay.sb2    -from 6   -to 20   -increment 1     -textvariable fsize             -width 4   -font {TkTextFont 14} -command [list font_callback]
        
        ttk::labelframe .top.frame.repos        -text "reposition X / Y"
        ttk::spinbox    .top.frame.repos.x      -from 100 -to 2000 -increment 25    -textvariable reposX -width 4   -font {TkTextFont 14}
        ttk::spinbox    .top.frame.repos.y      -from 100 -to 2000 -increment 25    -textvariable reposY -width 4   -font {TkTextFont 14} 
        button          .top.frame.repos.b1     -text " Reposition "                -command {repos_callback} 
        
        ttk::labelframe .top.frame.pause        -text "    Pause"
        button          .top.frame.pause.on     -text " All "        -command {do_pause on} 
        button          .top.frame.pause.off    -text " None "       -command {do_pause off} 
        
        ttk::frame      .top.frame.frame1
        ttk::frame      .top.frame.frame2
        ttk::frame      .top.frame.frame3
        ttk::frame      .top.frame.frame2.frame4
        
        button          .top.frame.frame3.b3           -text "Exit    "        -command {exit} -font {consolas 10}
        button          .top.frame.frame3.b4           -text "Send Cmd"    -command {tasks::send_command} -font {consolas 10}
        
        button          .top.frame.frame2.frame4.b5           -text "Save"        -command {save_layout} -font {consolas 10}
        button          .top.frame.frame2.frame4.b6           -text "Restore"    -command {restore_layout} -font {consolas 10}
        
        checkbutton     .top.frame.frame1.c1           -variable pause     -relief raised  -text "Pause    " -font {consolas 10}
        checkbutton     .top.frame.frame1.c2           -variable changes   -relief raised  -text "Color New" -font {consolas 10}
        checkbutton     .top.frame.frame2.c3           -variable ontop     -relief raised  -text "On Top" -command {do_ontop} -font {consolas 10}
        
        balloon::balloon .top.frame.frame3.b4          -text "Opens 2 windows, one to send commands\nand another to view results"
        balloon::balloon .top.frame.repos.b1    -text "Using X and Y spinboxes, will\nreposition/resize all putz windows"
        balloon::balloon .top.frame.frame1.c2          -text "Change color of changed fields, briefly\ntime depends on refresh interval"
        balloon::balloon .top.frame.delay       -text "Refresh in seconds and Font size 6-20"
        balloon::balloon .top.frame.pause       -text "Turn on or off all pause checkboxes \nin putz windows" -showdelay 200
        balloon::balloon .top.frame.frame1.c1          -text "Pauses this task monitor"
        balloon::balloon .top.frame.frame2.c3          -text "Keeps this window on top"
#       balloon::balloon xxxxx  -text ""
#       balloon::balloon xxxxx  -text ""
        balloon::balloon .top.frame.frame2.frame4.b5    -text "Save layout to .taskmonitor_layout in current directory"
        balloon::balloon .top.frame.frame2.frame4.b6    -text "Restore layout from .taskmonitor_layout in current directory"
        pack    .top.frame                                                  -side top   -expand 0 -fill x
        
        pack    .top.frame.delay .top.frame.delay.sb .top.frame.delay.sb2   -side left  -expand 0 -fill x
        pack    .top.frame.repos .top.frame.repos.x  .top.frame.repos.y     -side left  -expand 0 -fill x
        
        pack    .top.frame.repos.b1                                             -side left  -expand 1 -fill x
        pack    .top.frame.pause                                                -side left  -expand 0 -fill x
        pack    .top.frame.frame1    .top.frame.frame2    .top.frame.frame3     -side left  -expand 1 -fill both
        pack    .top.frame.frame3.b4 .top.frame.frame3.b3                       -side top   -expand 1 -fill x -pady 1
        
        pack    .top.frame.frame1.c2 .top.frame.frame1.c1                       -side top  -expand 1  -fill both          
        pack    .top.frame.frame2.c3  .top.frame.frame2.frame4                  -side top  -expand 1  -fill both  -pady 1
        pack    .top.frame.frame2.frame4.b5  .top.frame.frame2.frame4.b6       -side left  -expand 1  -fill both  
                
               
        pack    .top.frame.pause.on     .top.frame.pause.off                    -side left -expand 0  -fill x
        
        wm geom .top 892x318+128+128
        wm protocol .top WM_DELETE_WINDOW {putz "Can't close the monitor, pause and use minimize \nputz windows can be closed and reopened however" yellowonblack}
#       -----------------------------------------------------------------------------------------
        
#       ;proc xputs  {args} {
#       }
        set widths  {15   3   7     5      20     15     30    28     15    4   25  }       ;# column widths initially, user can resize
        set headers {Task row count Q-len rvar   share result error caller putz user}       ;# column headings
        
        
        set nrows $init_nrows                               ;# initial number of rows, now will add rows as needed, note this must be global for callbacks to work
        set ncols [expr {   [llength $headers] - 1   }]     ;# number of columns-1
        
        set once 0
        
        set path [create_frame .top]
        create_table .top $path $nrows $ncols $widths $headers $fsize
        
        set ntasks -1
        set maxflag 1
#       set t_task_pause 1
        proc do_pause {arg} {
            if { $arg eq "on" } {
                set p 1
            } else {
                set p 0 
            }
            set tnames [lsort -dictionary -stride 2 [array get ::table *,0]]
            dict for {ind tn} $tnames {
#               tasks::putz "ind = $ind, tn = $tn"
                if { $ind eq "0,0" || $tn eq "_taskmonitor" || $tn eq "" || $tn eq "sendcmd"} {
                    continue
                }
#               tasks::putz "                                 turn pause $arg for $ind / $tn   "
                set tid [tasks::tset $tn tid]
                if { ! [thread::exists $tid]} {
                    continue
                }
#               tasks::putz "                                  $tid"
                thread::send -async $tid "set t_task_pause $p"
                tsv::set tvar $tn,paused $p
            }
        }
        
        proc do_ontop {args} {
            tasks::putz "new on top $::ontop"   
            wm attributes .top -topmost $::ontop
        }



        
        proc setfont {size} {
#           tasks::putz "new font size $size"   
            for {set r 0} {$r <= $::nrows} {incr r} {
                for {set c 0} {$c <= $::ncols } {incr c} {
                    set w $::widget($r,$c)
                    $w configure -font "TkTextFont $size"
                }   
            }
        }
        
        proc font_callback {args} {
            setfont $::fsize
            event generate .top.fra.scframe.canvas <Configure> ;# should be able to compute this widget path, but...
        }
        proc repos_callback {} {
            tasks::repos ${::reposX}x$::reposY $::repos_rows
        }
        
#       ---------------------------- main monitoring loop -------------------------------------
        
        while 1 {
            tasks::tpause_check
            if { $pause } {
                tasks::wait 1000
                continue
            }
            set geom [wm geom .top]
            set tasks [tasks::tdump +,tid\t]
            set len [llength $tasks]
            if { $len != $ntasks } {
                set ntasks $len
            }
            
            set row 0
            foreach t $tasks {          ;# check each task for changes
                incr row
                if { $row > $nrows} {
                    add_row $row
                }
                if { $row > $nrows} { ;# if still true, something is wrong we will punt
                    if { $maxflag } {
                        set maxflag 0 ;# don't report this again
                        tasks::putz "Cannot process task $t row $row > max rows $nrows"
                        tasks::tset _taskmonitor error "Exceeded max rows, skipping..."
                    }
                    continue
                }
                set tname [lindex [split $t ,]   0 ]
                set table($row,0) $tname
                set temp [tasks::tset $tname tid]
                if { ! [thread::exists $temp] } {
                    $::widget($row,0) configure -bg pink    ;# indicate the thread has exited (probably an error)
                } else {
                    $::widget($row,0) configure -bg grey80  ;# otherwise back to normal
                }
                set column 0
                foreach item {row  count queue gvar share result error pid putz user} {
                    incr column
                    if { $item eq "row" } {
                        set ::table($row,$column) $row
                        if [catch {
                            if { [tsv::set tvar $tname,paused] } {
                                $::widget($row,$column) configure -bg pink
                            } else {
                                $::widget($row,$column) configure -bg grey97    
                            }
                        } err_code] {
                            $::widget($row,$column) configure -bg red
                            tsv::set tvar $tname,error $err_code    
                        }
                        continue
                    }
                    if { $item eq "queue" } {
                        set temp [tsv::llength tvar $tname,queue]   ;# current value
                        if { $temp == 0 } {
                            set temp ""
                        }
                        set tval $::table($row,$column)             ;# table value now
                        set ::table($row,$column) $temp             ;# new value
                    } elseif { $item eq "count" } {
                        set temp [tasks::tset $tname $item]         ;# current value
                        if { $temp == 0 } {
                            set temp ""
                        }
                        set tval $::table($row,$column)             ;# table value now
                        set ::table($row,$column) $temp             ;# new value
                    } elseif { $item eq "pid" } {
                        set temp [tasks::tset $tname $item]         ;# current value
#                       tasks::putz "item= |$item| temp= |$temp| tname= |$tname| row= |$row| column= |$column| "
                        if [catch {
                            set temp [tasks::tname $temp]
                        } err_code] {
                            set temp "$err_code"
                        }
                        set tval $::table($row,$column)     ;# table value
                        set ::table($row,$column) $temp     ;# update the table to the current
                    } else {
                        set temp [tasks::tset $tname $item] ;# current value
                        set tval $::table($row,$column)     ;# table value
                        set ::table($row,$column) $temp     ;# update the table to the current
                    }
                    
                    if { $temp != $tval } {
                        if { $changes } {
                            $::widget($row,$column) configure -bg pink  ;# show this changed
                        }
                    } else {
                        $::widget($row,$column) configure -bg grey97    ;# back to this if not changed
                    }
                }
            }
            if { $once == 0 } {
                wm geom .top 1495x312+2+2
                incr once
            }
            tasks::wait [expr {  int( $refresh_seconds * 1000 )   }]
#           wm withdraw .
        }
        
        
    } ;# end task def
    
#proc end task def

#   startup 
    tcall  $::_taskmonitor  ::tasks::mon_start   <- {*}$args
    tset _taskmonitor user "args: $args"
    return ""
} ;# end task_monitor proc

    proc send_command {} {
        tasks::Task sendcmd -import_tasks {{-package require Tk} tasks::twidgets tasks::task_monitor tasks::repos balloon::*} {
                
                ############################################
                # RS menu code from wiki (with my changes)
                ############################################
                
            proc menu:create {w menulist} {
                if {$w=="."} {set w2 ""} else {set w2 $w}
                menu $w2.menubar ; $w config -menu $w2.menubar
                foreach {hdr items tearoff} $menulist {menu:add $w $hdr $items $tearoff} ;# mine has a 3rd item for a tearoff 0/1
            }
            proc menu:add {w top descr {tearoff 0}} {
                if {$w=="."} {set w ""}
                set it $w.menubar.m$top
                if {![winfo exists $it]} {
                    menu $it -font {consolas 12} -tearoff $tearoff
                    $w.menubar add cascade -label $top -menu $it -underline 0
                }
                foreach {label cmd} $descr {
                    if {$label=="--"} {$it add separator; continue}
                    if {[regexp {^-(.+)} $label -> label]} {
                        set state disabled
                    } else {set state normal}
                    if ![catch {$it index $label}] continue ;# label was there
                    if {[regexp {^x (.+)} $label -> label]} {
                        regsub -all " " $label "_" xlabel
                        $it add check -label $label -state $state\
                                -variable ::$xlabel -command $cmd
                    } elseif {[regexp {^R (.+)} $label -> label]} {
                        catch {$it add cascade -label $label -menu $it.r$label}
                        set radi [menu $it.r$label -tearoff 0]
                        foreach {varname default} $cmd break
                        global $varname
                        set $varname $default
                        foreach {txt cmd} [lrange $cmd 2 end] {
                            $radi add radio -label $txt -variable $varname -command $cmd
                        }
                    } elseif {[regexp {^C (.+)} $label -> label]} {
                        catch {$it add cascade -label $label -menu $it.r$label}
                        set casc [menu $it.r$label -tearoff 0 -font {consolas 12}]
                        foreach {varname default} $cmd break
                        global $varname
                        set $varname $default
                        foreach {txt cmd} [lrange $cmd 0 end] {
                            if { $txt eq "--" } {
                                $casc add separator 
                            } else {
                                $casc add command -label $txt -command $cmd
                            }
                        }
                    } else {
                        $it add command -label $label -state $state -command $cmd -font {consolas 12}
                    }
                }
            }
            
            proc menu:delete {w top label} {
                if {$w=="."} {set w ""}
                set it $w.menubar.m$top
                catch {$it delete [$it index $label]}
            }
                
                ######################################################
                # RS history entry code from wiki (with my changes)
                #####################################################
                
            namespace eval history {}
                
            proc history::add? {w {this {}}} {
                variable $w
                variable n$w
                upvar 0 $w hist
                set s [set ::[$w cget -textvariable]]
                if {$s == ""} return
                if { $this ne "" } { ;# manual entry into history
                    lappend hist $this
                    set n$w [llength $hist]
                }
                if [string compare $s [lindex $hist end]] {
                    lappend hist $s
                    set n$w [llength $hist]
                } else {
                    set n$w [llength $hist] ;# correction, if used one from history, we want it on the next up
                }
            }
            proc history::save {w1 w2} {
                variable $w1
                variable n$w1
                upvar 0 $w1 hist1
                variable $w2
                variable n$w2
                upvar 0 $w2 hist2
                set io [open [file join [pwd] .sendcmd_history] w]
                foreach item $hist1 {
                    puts $io $item  
                }
                puts $io "###end###"
                foreach item $hist2  {
                    puts $io $item  
                }
                puts $io "###end###"
                close $io
            }
            proc history::restore {w1 w2} {
                variable $w1
                variable n$w1
                upvar 0 $w1 hist1
                variable $w2
                variable n$w2
                upvar 0 $w2 hist2
                
                set io [open [file join [pwd] .sendcmd_history] r]
                set data [split [read -nonewline $io] \n]
                set n 0
                foreach item $data {
                    if { $n == 0 } {
                        if { $item eq "###end###" } {
                            incr n
                            continue
                        } else {
                            putz "add item to history list $n: |$item| " green
                            lappend hist1 $item
                        }
                    } else {
                        if { $item eq "###end###" } {
                            break
                        } else {
                            putz "add item to history list $n: |$item| " green
                            lappend hist2 $item
                        }
                    }   
                }
                history::move $w1 99999;
                history::move $w2 99999;
            }
            proc history::move {w where} {
                variable $w
                variable n$w
                upvar 0 $w hist
                incr n$w $where
                if {[set n$w]<0} {set n$w 0}
                if {[set n$w]>=[llength $hist]+1} {
                    set n$w [llength $hist]
                }
                set ::[$w cget -textvar] [lindex $hist [set n$w]]
            }
            proc history::for {type name args} {
                switch -- $type {
                    entry {
                        uplevel $type $name $args
                        bind $name <Up> {history::move %W -1; %W selection clear; %W icursor end}
                        bind $name <Down> {history::move %W 1;%W selection clear; %W icursor end}
                        bind $name <Next> {history::move %W 99999; %W selection clear}
                        bind $name <Return> {history::add? %W ; %W selection clear}
                        variable $name {}
                        variable n$name 0
                    }
                    default {error "usage: history::for entry <w> <args>"}
                }
            }
                
#proc gui setup for sendcmd
                
            tasks::twait -> widget1 widget2
            set t_debug 0 ;# use debug window to log sent commands and return results
            
    #    ------------------------ gui setup ----------------------
            wm title . "Send Command"
            wm attributes .  -topmost 1 ; wm geom . 1000x69+10+10
            
            set ::tcl_wordchars {\S}
            set ::tcl_nonwordchars {[\[\]\{ \$\:\(\)\|\"\n\\/]}
            
            ttk::labelframe .f -text "Taskname (or pattern or tid)                      Send command"
            history::for entry .f$widget1 -textvar ent1 -font {consolas 12} -width 20 -relief groove -bd 5
            history::for entry .f$widget2 -textvar ent2 -font {consolas 14} -width 20 -relief groove -bd 5 -validate key -validatecommand {do_validate %d %P %s %V %W}
            button  .f.up                 -text " \u2191 "  -font {TkDefaultFont 14 roman}  -command [list event generate .f$widget2 <Up>]   -relief groove -bd 5
            button  .f.down               -text " \u2193 "  -font {TkDefaultFont 14 roman}  -command [list event generate .f$widget2 <Down>] -relief groove -bd 5
            button  .f.doit     -text "Send"  \
                                -command  "focus .f$widget2; event generate .f$widget2 <Return> ; event generate .f$widget2 <Up>" \
                                -relief groove -bd 5
            pack    .f                      -fill both -expand true  -side left
            pack    .f$widget1              -fill both -expand false -side left
            pack    .f$widget2              -fill both -expand true  -side left
            pack    .f.up .f.down .f.doit   -fill both -expand false -side left
            
            bind    .f$widget2 <Return> {+  do_send $ent1 $ent2}
            bind    .f$widget1 <MouseWheel>         {+  do_wheel %D 1}
            bind    .f$widget2 <MouseWheel>         {+  do_wheel %D 2}
            
            bind    .f$widget1 <Button-4>           {+  do_wheel 1 1}
            bind    .f$widget1 <Button-5>           {+  do_wheel -1 1}
            bind    .f$widget2 <Button-4>           {+  do_wheel 1 2}
            bind    .f$widget2 <Button-5>           {+  do_wheel -1 2}
            
#           bind    .f$widget2 <<Paste>>            {+ }             ;# using validate instead of this
            bind    .f$widget2 <F1>                 {+  do_extend 1} ;# for now, we only handle extending, not shrinking
            bind    .f$widget2 <Shift-Key-Escape>   {+  do_fill 1}
            bind    .f$widget2 <Key-Escape>         {+  do_fill 0}
            bind    .f$widget2 <Key-Tab>            {+  do_tab %W ; break}
            
set fhelp {Send a command to a task or thread. If the task entry is blank
it will use the main thread. Wild cards are only used for tasks.

Use an <Escape> to finish 1 square-bracket,  paren, or brace at the
right end. <Shift-Escape> will finish them all. After finishing,
the text between the brackets is selected so be careful. F1 will 
extend the selection, if any, by one char on each side.

The <tab> key will attempt to fill in variable names or commands
The mousewheel can also be used here for the up/down history.}

            balloon::balloon .f             -dismissdelay 40000 -showdelay 2000 -text $fhelp
            
            balloon::balloon .f.up          -text "Go back to previous commands from history"
            balloon::balloon .f.down        -text "Go forward to next command in history"
            balloon::balloon .f.doit        -text "This will send a command and do an <up>"

    #    ------------------------ gui setup end ------------------
        
#           tasks::putz "widget2 .f$widget2"
            tasks::putz "Results from sent commands output here.\nTask putz output goes to separate putz windows\nThe widget tree option requires BWidgets"
            
            set ::Always_on_Top 1
            set ::Add_Separator_Line 1
            set ::all_tasks {}
            set ::Color green
            proc  do_validate {action new old type widget} {
                set temp [string map  {"\t" " "} $new]
                if { $temp eq $new } {
                } else {
                    putz "cannot have tabs in a pasted string, changing to spaces" red
                    after idle [list set ::ent2 $temp]
                }
                $widget selection clear
                return 1
            }
            proc closeem {line} {
                set positions {}
                set out {}
                set in [lreverse [split $line {}]]
                set n [string length $line]
                set paren 0
                set brace 0
                set bracket 0
                set quote 0
                set ob "\{"
                set cb "\}"
            
             
                foreach char $in {
                    incr n -1
                    if       { $char eq "\"" } {
                        if { $quote } {
                            set quote 0
                        } else {
                            set quote 1 
                        }
                    } elseif { $quote > 0 } {
                    } elseif {  $char eq "\(" } {
                        if { $paren > 0 } {
                            incr paren -1
                        } else {
                            append out ")"  ; lappend positions $n
                        }
                    } elseif {  $char eq "\[" } {
                        if { $bracket > 0 } {
                            incr bracket -1
                        } else {
                            append out "\]" ; lappend positions $n
                        }
                    } elseif {  $char eq $ob } {
                        if { $brace > 0 } {
                            incr brace -1
                        } else {
                            append out $cb  ; lappend positions $n
                        }
                        
                    } elseif {  $char eq "\)" } {
                        incr paren
                    } elseif {  $char eq "\]" } {
                        incr bracket
                    } elseif {  $char eq $cb } {
                        incr brace
                    } else {
                    }
                }
                    return [list $out $positions]
            }
            proc do_fill {all} {
                set c [closeem $::ent2]
                lassign $c out positions
                if {! $all } {
                    set out [string index $out 0]
                    set positions [lindex $positions 0]
                } else {
                }
                set n -1
                set before $::ent2
                foreach p $positions {
                    set out1 [string index $out [incr n]]
                    set ::ent2 "$::ent2$out1"
                    .f$::widget2  icursor end
                    set endstr [expr {   [string length $::ent2]-1   }]
                    .f$::widget2 selection clear
                    .f$::widget2 selection from $endstr
                    .f$::widget2 selection to [expr {   $p+1   }]
                    if { $n+1 < [llength $positions] } {
                        wait 150
                    } else {
                    }
                }
                if { $before ne $::ent2 } {
                    history::add? .f$::widget2 $before
                }
            }
            proc do_extend {direction} {
                if [catch {
                    set first [.f$::widget2 index sel.first]
                    set last  [.f$::widget2 index sel.last]
                    set len [string length $::ent2]
                    if { $first < 1 || $last > $len-1 || $first >= $last } {
                        putz "Cannot extend selection (on both ends)" red
                        return
                    }
                    .f$::widget2 selection from [expr {   $first-1   }]
                    .f$::widget2 selection to   [expr {   $last+1   }]
                } err_code] {
                    putz "extend error: $err_code" red
                    return
                }
            }
            
            
####################### tab fill in ##########################################################          
            
proc EvalAttached {args} {
    global ent1 ent2
    set result ""
    set use ""
    if       { $ent1 eq ""  && [string match -nocase *tclsh* [info nameof]] == 0} { ;# if blank try to get mainthread tid
        if [catch {
            set use [tsv::set main mainthread]
        } err_code] {
            error "cannot find mainthread tid: $err_code "
            return ""
        }
    } else { ;# not blank, check if a task name first, then an existing and valid tid
        if [catch {
            set use [tset $::ent1 tid] ;# is it a valid taskname
        } err_code] {
            if [catch {
                set atid [thread::exists $::ent1]
                if { $atid } {
                    set use $::ent1
                } else {
                    error "thread does not exist tid: $ent1"    
                }
            } err_code] {
#                error "cannot use tid $use: $err_code / $::all_tasks"
                set use [tset [lindex $::all_tasks 0] tid]
            }
        }
    }
    if [catch {
        set zzz [thread::send $use [list uplevel #0 {*}$args]]
        set result $zzz
    } err_code err_dict] {
        error "cannot send to $use, $err_code $err_dict"
    }
    return $result
}


proc ExpandProcname str {
    set match [EvalAttached [list info commands $str*]]
    if {[llength $match] == 0} {
        set ns [EvalAttached \
                "namespace children \[namespace current\] [list $str*]"]
        if {[llength $ns]==1} {
            set match [EvalAttached [list info commands ${ns}::*]]
        } else {
            set match $ns
        }
    }
    if {[llength $match] > 1} {
        regsub -all { } [ExpandBestMatch $match $str] {\\ } str
        set match [linsert $match 0 $str]
    } else {
        regsub -all { } $match {\\ } match
    }
    if       { [llength $match] == 1  } {
        return $match
    } elseif { [llength $match] > 1  } {
        return [lrange $match 1 end]
    } elseif { [llength $match] == 0  } {
        return ""
    } else {
        error "cannot happen match length error"
    }
    
}

# borrowed from the console code, we cannot do all of it since 
# an entry has less abilities than a text widget

proc ExpandVariable str {
    if {[regexp {([^\(]*)\((.*)} $str -> ary str]} {
        ## Looks like they're trying to expand an array.
        set match [EvalAttached [list array names $ary $str*]]
        set match [lsort -dictionary $match]
        if {[llength $match] > 1} {
            set vars $ary\([ExpandBestMatch $match $str]
            foreach var $match {
                lappend vars $ary\($var\)
            }
            return [lrange $vars 1 end]
        } elseif {[llength $match] == 1} {
            set match $ary\($match\)
            return $match
        }
        ## Space transformation avoided for array names.
    } else {
        set match [EvalAttached [list info vars $str*]]
        if {[llength $match] > 1} {
            regsub -all { } [ExpandBestMatch $match $str] {\\ } str
            set match [linsert $match 0 $str]
        } else {
            regsub -all { } $match {\\ } match
            return $match
        }
    }
    return [lrange $match 1 end]
}

proc ExpandBestMatch {l {e {}}} {
    set ec [lindex $l 0]
    if {[llength $l]>1} {
        set e [expr {[string length $e] - 1}]
        set ei [expr {[string length $ec] - 1}]
        foreach ll $l {
            while {$ei>=$e && [string first $ec $ll]} {
                set ec [string range $ec 0 [incr ei -1]]
            }
        }
    }
    return $ec
}


proc choose {w choices {start 0} {max 20} {kind ?}} {
    unset -nocomplain ::the_choice
    catch {destroy .p}
    menu .p -tearoff 0
    update ; # <===========================================================
    .p  add command -label "-none- [string range $kind 6 end]" -command "set ::the_choice {-none-} " -font {arial 12}
    set n $start
    foreach choice  [lrange [lsort -dictionary $choices]  $start  [expr {   $start+$max-1   }]] {
        if { $choice eq "" } {
            continue
        }
        .p  add command -label "[incr n] $choice" -command "set ::the_choice $choice " -font {arial 12}
    }
    if { $start+$max < [llength $choices]} {
        .p  add command -label "-next-" -command "set ::the_choice {-next-} " -font {arial 12}
    }
    set coords [lrange [split [wm geom .] +] 1 end]
    set zzz [tk_popup .p {*}$coords 1]
#                                                                                   update
    if {! [info exist ::the_choice] } {
        vwait  ::the_choice
    }
    return $::the_choice
    
}

proc getchoice {w choices kind} {
    if { [llength $choices] == 1 } {
        set ::the_choice $choices
        return $choices
    }
    set next 0
    set max 20
    while { 1 } {
        choose $w $choices $next $max $kind
        wait 100
        if { ![info exist ::the_choice] } {
            set ::the_choice "-none-"
        }
        if       { $::the_choice eq "-none-" } {
            set ::the_choice ""
            break
        } elseif { $::the_choice eq "-next-" } {
            incr next $max
            if { $next > [llength $choices] } {
                set ::the_choice ""
                break
            }
            continue
        } else {
            break
        }
    }
    return $::the_choice
}


proc do_tab {window} {                      ;# callback for a tab char
    set cur [$window index insert]
    set str [$window get]
    set sym [string range $str 0 $cur-1]
    set rev [string reverse $sym]
    
    
    regexp -nocase -linestop -lineanchor {([^\[^\\\] \t\n\r\}\{\"\$\)]*)(.)?} $rev -> result  prior
    set astr [string reverse $result]
    set pos1 [expr {   $cur - [string length $astr]   }]
    if { $astr eq "" } {
        return
    }
    
    set none 0
    set before $::ent2
    foreach kind {ExpandVariable ExpandProcname} {  ;# don't bother with the path name expand
        if { $pos1 == 0 && $kind eq "ExpandVariable" } { ;# we don't try if the word begins in col 0
            continue
        } elseif {$prior eq "\[" && $kind eq "ExpandVariable"} { ;# and if it is preceeded by an open bracket, we assume it's a command
            continue
        }
        set rep1 [$kind $astr]
        
        if { $rep1 eq ""  || [string length $rep1] < [string length $cur]} {
            continue
        } elseif {[llength $rep1] == 0} {
            set replacement ""
            continue
        } elseif {[llength $rep1] > 1} {
            set c [getchoice $window $rep1 $kind]           ;# more than 1, give user a choice
            if { $c eq "" } {
                return
            }
            set replacement $c
            incr none
            break
        } elseif {[llength $rep1] == 1} {
            set bg [$window cget -bg]               ;# the best we can do is flash the background, green is for ok
            $window configure -bg LightGreen
            wait 250
            $window configure -bg $bg
            set replacement $rep1
            incr none
            break
        }
    }
    if { $none == 0 } {
        set bg [$window cget -bg]                   ;# if we couldn't find anything, flash pink (red is too dark)
        $window configure -bg pink
        wait 250
        $window configure -bg $bg
        return
    }
    
    set new [string replace $str $pos1 $cur-1 $replacement]
    set rlen [string length $replacement]
    set olen [string length $result]
    set move [expr {   $rlen - $olen  }]
    $window delete 0 end
    $window insert 0 $new
    set newpos [expr {    $move + $cur   }]
    $window icursor $newpos
    if { $before ne $::ent2 } {
        history::add? .f$::widget2 $before
    } else {
    }
}
            
            
        
#proc #################################################################################         
            
            proc do_send {ent1 ent2} {
                set rcolor $::Color
                putz "ent1= |$ent1| ent2= |$ent2| " yellowonblack
                if { $ent1 eq "" && [string match -nocase *tclsh* [info nameof]] == 0} { 
                    set ::ent1 [tsv::set main mainthread]
                    set ent1 $::ent1
                }
                if { $ent1 eq "sendcmd" || $::ent1 eq [tset sendcmd tid]} {
                    set ::ent1 [tset sendcmd tid]
                    set ent1 $::ent1
                    after 2000 {wm deiconify .}
                }
                if { $ent1 eq "taskmonitor"  || $ent1 eq "_taskmonitor"} {
                    set ::ent1 [tset _taskmonitor tid]
                    set ent1 $::ent1
                }
                
                if [catch {
                    set atid [thread::exists $ent1]
                } err_code] {
                    set atid 0
                }
                if { ! $atid } {
                    set senders 0
                    foreach t $::all_tasks {
                        set mat 0
                        if       { [string index $ent1 0] eq "-" } { ;# use regexp match on 1-end
                            if { [regexp [string range $ent1 1 end] $t] } {
                                set mat 1
                            }
                            
                        } elseif { [string index $ent1 0] eq "#"  } { ;# use range match
                            set r [split [string range $ent1 1 end] "-"]
                            if       { [llength $r ] == 1 } {
                                set from [lindex $r 0]
                                set to   $from
                            } elseif { [llength $r ] == 2 } {
                                set from [lindex $r 0]
                                set to   [lindex $r 1]
                                if { $to eq "" } {
                                    set to 9999 ;# this is N- so assume N-end
                                }
                                if { $from eq "" } {
                                    set from 0 ;# this is N- so assume N-end
                                }
                            } else {
                                error "Bad # range: $ent1"
                            }
                            set zzz [regexp -nocase -linestop -lineanchor {[^0-9](\d+)} [string range $t 1 end] -> number]
                            if {$zzz & $number >= $from && $number <= $to} {
                                set mat 1
                            }

                        } else {
                            if { [string match -nocase $ent1 $t]} {
                                set mat 1
                            }
                        }
                        if { $mat } {
                            incr senders
                            putz "thread::send [tset $t tid] / $t \{$ent2\}"; 
                            if {$ent2 ne ""} {
                                set bline ""
                                if { $::Add_Separator_Line } {
                                    set bline {tasks::putz "" green ;}
                                }
                                set zzz [thread::send [tset $t tid] $bline$ent2]
                                set vline "\u250B"
                                putz "return from $t: $vline$zzz$vline" $rcolor
                            } else {
                                putz "empty not sent";set zzz ""
                            } ;
                        } else {
                        }   
                    }
                    if { $senders == 0 } {
                        putz "No tasks or tid for $ent1" rederror
                    }
                } else { ;# it's a tid
                    if {$ent2 ne ""} {
                        set bline ""
                        if { $::Add_Separator_Line } {
                            set bline {tasks::putz "" green ;}
                        }
                        set zzz [thread::send $ent1 $bline$ent2]
                        set vline "\u250B"
                        putz "return from $ent1: $vline$zzz$vline"  $rcolor
                    } else {
                        putz "empty - not sent";set zzz ""
                    } ;
                }
#               event generate .f$::widget1 <Return> ;# so the task/tid entry gets into the history
                history::add? .f$::widget1
                set ::ent2 "";
                return
            }

            proc do_ontop {args} { ;# toggle on top
                wm attributes . -topmost $::Always_on_Top
            }
            proc do_minimize {hide} { ;# minimize or un-minimize window
                if { $hide } {
                    set ::ent2 {wm withdraw .taskdebug ;# sent this}
                } else {
                    set ::ent2 {wm deiconify .taskdebug ;# sent this}
                }
                .f.doit invoke
            }
            proc do_wheel {direction which} {
                if { $which == 2 } {
                    focus -force .f$::widget2
                    if { $direction < 0 } {
                        event generate .f$::widget2 <Down>
                    } else {
                        event generate .f$::widget2 <Up>
                    }
                } else {
                    focus -force .f$::widget1
                    if { $direction < 0 } {
                        event generate .f$::widget1 <Down>
                    } else {
                        event generate .f$::widget1 <Up>
                    }
                }
            }
            proc do_widget_tree {args} {
                set ::ent2 {tasks::twidgets ;# sent this}
                .f.doit invoke
            }
            proc do_clear {args} {
                set ::ent2 {.taskdebug.ttttt delete 1.0 end ;# sent this}
                .f.doit invoke
            }
            proc do_send_now {n arg } {
                if       { $n >= 1 && $n <= 4} {
                    set ::ent2 $arg
                    .f.doit invoke
                } elseif { $n == 5 } {
                    set ::ent2 $arg
                    .f.doit invoke
                } elseif { $n == 6 } {
                    dothis
                } else {
                    error "bad do_send_now $n"
                }
            }
            proc do_see_end {args} {
                set ::ent2 {.taskdebug.ttttt see end ;# sent this}
                .f.doit invoke
            }
            proc do_tlg {arg} {
                
                if       { $arg == 1 } {
                    set ::ent2 {tasks::tlg }
                    focus -force .f$::widget2 
                    .f$::widget2 icursor end
                } elseif { $arg == 2 } {
                    set ::ent2 {tasks::tlg * \u250B 99 ;# * is a glob pattern}
                    focus -force .f$::widget2 
                    .f$::widget2 selection range 11 12
                    .f$::widget2 icursor 12
                } elseif { 0 } {
                    dothis
                } elseif { 0 } {
                    dothis
                } else {
                    dothis
                }
            }
            proc do_tla {arg} {
                if       { $arg == 1 } {
                    set ::ent2 {tasks::tla array}
                    focus -force .f$::widget2 
                    .f$::widget2 selection range 11 end
                    .f$::widget2 icursor end
                } elseif { $arg == 2 } {
                    set ::ent2 {tasks::tla array *}
                    focus -force .f$::widget2 
                    .f$::widget2 selection range 11 16
                    .f$::widget2 icursor 16
                } elseif { $arg == 3 } {
                    set ::ent2 {tasks::tla array * 1}
                    focus -force .f$::widget2 
                    .f$::widget2 selection range 11 16
                    .f$::widget2 icursor 16
                } elseif { 0 } {
                    dothis
                } else {
                    dothis
                }
            }
            proc do_font {size} {
                global widget2
                if { $size eq "tiny"  } {
                    .f$widget2 configure -font {consolas 9}
                } elseif       { $size eq "small" } {
                    .f$widget2 configure -font {consolas 11}
                } elseif { $size eq "medium"  } {
                    .f$widget2 configure -font {consolas 14}
                } elseif { $size eq "large"  } {
                    .f$widget2 configure -font {consolas 16}
                } else {
                    error "bad font menu"
                }
            }
            proc do_load_save {arg w1 w2} {
                if { $arg } {
                    history::save $w1 $w2
                    putz "save history to [file join [pwd] .sendcmd_history] |$w1| |$w2|"
                } else {
                    history::restore $w1 $w2
                    putz "redload history from [file join [pwd] .sendcmd_history] |$w1| |$w2|"  
                }
            }
            
            proc do_refresh_tasks_menu {args} {
                set tasks [tdump +,tid\t ]
                
                set tt {}
                foreach t $tasks {
                    lassign [split $t ,] name tid
                    if { ($name eq "_taskmonitor" || $name eq "sendcmd" ) && ![info exist ::t_overide] } {
                        continue
                    }
                    lappend tt $name
                }
                if { $::all_tasks == $tt } {
                    after 2000 do_refresh_tasks_menu
                    return
                }
                putz "Task menu updated to: ($tt)" green
                
                set ::all_tasks {}
                foreach t $tasks {
                    lassign [split $t ,] name tid
                    menu:delete . Task $name
                }
                foreach t $tasks {
                    lassign [split $t ,] name tid
                    if { ($name eq "_taskmonitor" || $name eq "sendcmd" ) && ![info exist ::t_overide] } {
                        continue
                    }
                    menu:add . Task [list $name "set ent1 $name; history::add? .f$::widget1"  ]
                    lappend ::all_tasks $name
                }
                after 2000 do_refresh_tasks_menu
            }
            proc do_lookup {args} {
                set arg $::ent2
                if { [string range $::ent2 0 1] eq "::" } { ;# since tab sometimes fills in the leading ::, this should help, magicsplat can't find it with leading ::
                    set arg [string range $::ent2 2 end]
                }
                if {$::tcl_platform(platform) ne "windows"   } {
                    exec xdg-open "https://www.magicsplat.com/tcl-docs/docindex.html?search=$arg" &

                } else {
                    exec cmd.exe /c start "" "https://www.magicsplat.com/tcl-docs/docindex.html?search=$arg" &
                }
            }
            
#           we added a 3rd item for each menu, a tearoff boolean, for Task we populate dynamically, so start off empty
            menu:create . {
                Task {
                } 1
                Commands {
                    "tlg"                           {do_tlg 1}
                    "tlg pattern delim max-width"   {do_tlg 2}
                    -- {}
                    "tla array"                     {do_tla 1}
                    "tla array *"                   {do_tla 2}
                    "tla array * match-data"        {do_tla 3}
                    -- {}
                    "Wiget Tree Tool"               {do_widget_tree}
                    -- {}
                    "C putz commands" {
                        "See   putz Win at end"     {do_see_end}
                        "Hide  putz Win"            {do_minimize 1}
                        "Show  putz Win"            {do_minimize 0}
                        "Clear putz Win"            {do_clear}
                        "Enable Tk putz linux"      {do_send_now 1 {set ::t_debug 0x4; tasks::putz "Enable Tk on Linux"}}
                    }
                    "C Clear Task Fields" {
                        "Counts"                    {do_send_now 1 {tasks::tset $::t_name count 0  ;# sent}}
                        "Results"                   {do_send_now 1 {tasks::tset $::t_name result ""  ;# sent}}
                        "Errors"                    {do_send_now 1 {tasks::tset $::t_name error "" ;# sent}}
                        "User"                      {do_send_now 1 {tasks::tset $::t_name user ""  ;# sent}}
                        -- --
                        "All the Above"             {do_send_now 4 {tasks::tset $::t_name count 0 ;tasks::tset $::t_name result ""  ;tasks::tset $::t_name error "" ;tasks::tset $::t_name user ""  ;# sent}}
                    }
                    "C Misc commands" {
                        "Pause on"                  {do_send_now 5 {set ::t_task_pause 1 ; tsv::set tvar $::t_name,paused 1  }}
                        "Pause off"                 {do_send_now 5 {set ::t_task_pause 0 ; tsv::set tvar $::t_name,paused 0  }}
                        "Toggle Pause"              {do_send_now 5 {set ::t_task_pause [expr {1 - $::t_task_pause}] ; tsv::set tvar $::t_name,paused $::t_task_pause }}
                        -- --
                        "Putz on"                   {do_send_now 1 {set ::t_putz_output 1 ;# sent}}
                        "Putz off"                  {do_send_now 1 {set ::t_putz_output 0 ;# sent}}
                        "Toggle Putz"               {do_send_now 1 {set ::t_putz_output [expr {1 - $::t_putz_output}] ;# sent}}
                    }
                    -- {}
                    "Lookup with browser"           {do_lookup}
                    "x Add Separator Line"          {}
                    "Clear Command Entry"           {set ::ent2 ""}
                } 1
                Extra {
                    "Refresh Task Menu"             {do_refresh_tasks_menu}
                    "x Always on Top"               {do_ontop}
                    -- {}
                    "R Font-Size" {
                        Font-Size medium
                        tiny {do_font tiny}
                        small {do_font small}
                        medium {do_font medium}
                        large {do_font large}
                    }
                    "R Color" {
                        Color green
                        normal {set ::color normal}
                        green  {set ::color green}
                        red    {set ::color red}
                    }
                    -- --
                    "Run Task Monitor"          {tasks::task_monitor}
                    -- --
                    "Save History"              {do_load_save 1 .f.t .f.w}
                    "Reload History"            {do_load_save 0 .f.t .f.w}
                    -- --
                    "Exit"                          {exit}
                } 0
            }
            do_refresh_tasks_menu ;# also first time create too
            wm protocol . WM_DELETE_WINDOW {putz "Can't close sendcmd, use minimize \nputz windows can be closed and reopened however" yellowonblack}

            tasks::treturn ok
            thread::wait
        }
        tasks::tcall $::sendcmd ok <- .t .w       ;# this starts up the sendcmd task
    
    }   ;# end proc send_command    

# widget tree viewer, needs Bwidgets, must add tasks::twidgets to the import list, otherwise this is not imported to task
proc twidgets {} {
    
    set ::tasks::wtreescript {
        proc wtree_:_node_openclose {which} {
            set nodes [.wtree_top.sw.t nodes root]
            if { $which == "open" } {
                foreach item $nodes {
                    .wtree_top.sw.t opentree $item
                }
                
            } else {
                foreach item $nodes {
                    .wtree_top.sw.t closetree $item
                }
                
            }
        }
        
        proc _wtree_ {{root .} {level 0}} {
            set top .wtree_top
            if {  $level == 0} {
                package require BWidget
                catch {
                    $top.sw.t delete [$top.sw.t nodes root]
                    destroy $top
                }
                toplevel $top
                frame $top.f
                button $top.f.b1 -text "Open" -command {tasks::wtree_:_node_openclose open}
                button $top.f.b2 -text "close" -command {tasks::wtree_:_node_openclose close}
                button $top.f.b3 -text "refresh" -command {tasks::_wtree_}
                pack $top.f  -side top -fill x
                pack $top.f.b1 $top.f.b2 $top.f.b3 -side left -expand yes -fill both
                ScrolledWindow $top.sw
                pack $top.sw -fill both -expand 1 -side top
                Tree $top.sw.t  -deltay 25 -deltax 25 -padx 5 -borderwidth 8 -linesfill orange -padx 5
                #pack $top.sw.t
                $top.sw setwidget $top.sw.t   ;# Make ScrolledWindow manage the Tree widget
                update                ;# Process all UI events before moving on.
                $top.sw.t bindText <1> +tasks::wtree_:_node_puts
                set ::wtree_queued_inserts {}
                wm geom $top 466x326+52+52
                if { [string range $::t_debug 0 1] eq "0x"} {
                    wm withdraw .
                }
                catch {wm title $top "Widgets: $::t_name"}
                
            }
            set children [winfo children $root]
            set class [winfo class $root]
            set info ""
            if       { $class == "TCheckbutton" || $class == "Checkbutton" ||  $class == "Button"  ||  $class == "TLabelframe" ||  $class == "TButton" } {
                set info [split [$root cget -text] \n]
            } elseif { $class == "TEntry" } {
                set info "var: [$root cget -textvariable]"
            } elseif { $class == "TButton2" } {
                set info [split [$root cget -text] \n]
            } else {
                
            }
            
            set root [regsub -all : $root _]
            set parts [split [string range $root 1 end] .]
            if { $parts == "" } {
                set parent root
            } else {
                set parent {}
                foreach item [lrange $parts 0 end-1] {
                    append parent .$item
                }
            }
            if { $parent == "" } {
                set parent root
            }
            set cmd "$top.sw.t insert end \{$parent\}   \{$root\}       -font {courier 12} -text  \{$root - $class $info\}"
            lappend ::wtree_queued_inserts $cmd
            
            
            
            if { $children == "" } {
                return $root
            } else {
                foreach child $children {
                    set tout [_wtree_ $child [expr ( $level + 1 )]]
                }
                
            }
            if { $level == 0 } {
                foreach item $::wtree_queued_inserts {
                    eval $item
                }
            }
        }
        
        proc wtree_:_node_puts {args} {
            #   return
            tasks::putz ""
            tasks::putz  $args green
            wtree_:_node_lw $args
            catch {tasks::putz "[pack info $args]" green}
            #   clipboard clear ; clipboard append $args
        }
        proc wtree_:_node_lw {widget} {   # list a widget
            set w [$widget configure]
            foreach item $w {
                set opt [lindex $item 0]
                set val "---"
                catch {set val [$widget cget $opt]}
                set wid($opt) $val
            }
            #la wid
            set vline "\uFFE8 "
            set names [lsort -dictionary [array names wid]]
            set n [llength $names]
            set n2 [expr ( $n/2 )]
            set odd [expr ( $n % 2 )]
            if { $odd } {
                incr n2  ;# so this one is 1 more than half
            }
            if { $odd } {
                for {set m 0;set m2 [expr ( $m+$n2 )]} {$m < $n2} {incr m;incr m2} {
                    
                    if { $m == $n2-1 } {
                        set left [lindex $names $m]
                        set leftt [format {%-20s %-20s} $left $wid($left)]
                        tasks::putz "$leftt${vline}"
                        
                    } else {
                        set left [lindex $names $m]
                        set right [lindex $names $m2]
                        set leftt [format {%-20s %-20s} $left $wid($left)]
                        if { [string length $leftt] > 41 } {
                            set leftt [format {%s %s} $left $wid($left)]
                            set leftt [string range [format %-41s $leftt] 0 40]
                        }
                        set rightt [format {%-20s %-20s} $right $wid($right)]
                        tasks::putz "$leftt${vline}[string trimright $rightt]"
                    }
                }
                
            } else {
                for {set m 0;set m2 [expr ( $m+$n2 )]} {$m < $n2} {incr m;incr m2} {
                    set left [lindex $names $m]
                    set right [lindex $names $m2]
                    set leftt [format {%-20s %-20s} $left $wid($left)]
                    if { [string length $leftt] > 41 } {
                        set leftt [format {%s %s} $left $wid($left)]
                        set leftt [string range [format %-41s $leftt] 0 40]
                    }
                    set rightt [format {%-20s %-20s} $right $wid($right)]
                    tasks::putz "$leftt${vline}[string trimright $rightt]"
                }
                
            }
        }
        
    } ;# ::tasks::wtreescript
    
    eval $::tasks::wtreescript
    tasks::_wtree_
} ;# twidgets

proc tla {array_name {pattern *} {reverse 0} } { # list an array, if reverse, pattern is against data in array
    upvar 1 $array_name array
    if {![array exists array]} {
        error "\"$array_name\" isn't an array"
    }
    set maxl 0
    set pat $pattern
    if { $reverse != 0} { ;# use max width of all since pattern will not be the index, but the data
        set pat *
    }
    foreach name [lsort [array names array $pat]] {
        if {[string length $name] > $maxl} {
            set maxl [string length $name]
        }
    }
    set maxl [expr {$maxl + [string length $array_name] + 2}]
    if {$reverse == 0} {;# match against index
        foreach name [lsort -dictionary [array names array $pattern]] {
            set nameString [format %s(%s) $array_name $name]
            tasks::putz [format "%-*s = %s" $maxl $nameString $array($name)]
        }
    } else { ;# match against data, not the index
        foreach name [lsort [array names array]] {
            set mat [string match "*$pattern*" $array($name)]
            set outputit 0
            if {$mat && $reverse > 0} {
                set outputit 1
            } elseif {!$mat && $reverse < 0} { ;# this means it did NOT match, and if reverse is negative, then we want those not matching
                set outputit 1
            }
            if { $outputit } {
                set nameString [format %s(%s) $array_name $name]
                tasks::putz [format "%-*s = %s" $maxl $nameString $array($name)]
            }
        }
        
    }
}



proc tlg {{pat **} {delimeter |} {max 80}} {          # list globals in threads
    if { ![info exist ::___tlg___ ] } {
        set ::___tlg___  [info globals] ;lappend ::___tlg___  ___tlg___ 
    }
    set a [lsort -dictionary [info global ${pat}*]]
    foreach gvar $a {
        if { $gvar in $::___tlg___  && $pat eq "**"} {
            continue
        }
        if {[array exists ::$gvar]} { ;# it is an array get some indices
            set val "() [lsort -dictionary [array names ::$gvar]]"
        } elseif { [info exists ::${gvar}] } {
            set val ${delimeter}[set ::${gvar}]$delimeter
            regsub -all {\n} $val [apply {code {eval set str "\\u[string map "U+ {}" $code]"}} 2936] val ;# or 21B2
        } else {
            continue ;# skip if we cant get the value
        }
        catch {
            tasks::putz [format "--- %-20s = %s" $gvar [string range $val 0 $max]]
        }
    }
}
# from the wiki
 namespace eval ::balloon {
   proc this {} "return [namespace current];";
 
   variable state;
 
   array unset state;
   array set state {};
 
   proc balloon {w args} {
     variable state;
 
     if {[info exists state($w.background)]} {
       foreach var [array names $w.*] {
         set [lindex [split $var "."] end] $state($var);
       }
     } else {
       set background   lightyellow;
       set dismissdelay 10000;
       set foreground   black;
       set label        "";
       set showdelay    1500;
       set text         "";
       set textvariable "";
     }
 
     foreach {option value} $args {
       set var  [string range $option 1 end];
 
       switch -exact -- $option {
         -bg         -
         -background -
         -fg         -
         -foreground {
           if {[string match "f*" $var]} {
             set var  foreground;
           } else {
             set var  background;
           }
 
           if {[catch {winfo rgb $parent $value;}]} {
             error "expected valid $var colour name or value, but got \"$value\"";
           }
         }
         -dismissdelay -
         -showdelay    {
           if {![string is integer -strict $value]} {
             error "expected integer delay value in ms, but got \"$value\"";
           }
         }
         -label        {}
         -text         {}
         -textvariable {}
         default  {
           error "bad option \"$option\": must be -background, -dismissdelay, -foreground, -label, -showdelay, or -text";
         }
       }
 
       set $var  $value;
     }
 
     array unset state $w.*;
 
     if {$showdelay == -1} {
       bind $w <Any-Enter> {};
       bind $w <Any-Leave> {};
       return;
     }

     set state($w.background)   $background;
     set state($w.foreground)   $foreground;
     set state($w.dismissdelay) $dismissdelay;
     set state($w.label)        $label;
     set state($w.showdelay)    $showdelay;
     set state($w.text)         $text;
     set state($w.textvariable) $textvariable;
 
 # FIX by [Vitus Wagner]
    if {$showdelay} {
      bind $w <Any-Enter> [list \
         after \
           $showdelay \
           [concat [namespace code showCB] %W] \
       ];
       bind $w <Any-Leave> [concat [namespace code destroyCB] %W];
     }
 
     return;
   }
 
   proc destroyCB {w} {
     variable state;
 
     catch {destroy $w.balloon;};
 
     if {[info exists state($w.id)] && ($state($w.id) != "")} {
       catch {after cancel $state($w.id);};
 
       set state($w.id)  "";
     }
 
     return;
   }
 
   proc showCB {w} {
     if {[eval winfo containing [winfo pointerxy .]] != $w} {
       return;
     }
     
     variable state;
 
     set top    $w.balloon;
     set width  0;
     set height 0;
 
     catch {destroy $top;}

    if {!$state($w.showdelay)} {
      return;
    }
 
     toplevel $top \
       -relief      solid \
       -background  $state($w.foreground) \
       -borderwidth 1;
 
     wm withdraw         $top;
     wm overrideredirect $top 1;
     wm sizefrom         $top program;
     wm resizable        $top 0 0;
     wm attributes       $top -topmost 1;# to force it if the window is topmost, else we behind it and hidden
 
     if {$state($w.label) != ""} {
       pack [label $top.label \
         -text       $state($w.label) \
         -background $state($w.background) \
         -foreground $state($w.foreground) \
         -font       {{San Serif} 10 bold} \
         -anchor     w \
         -justify    left \
       ] -side top -fill x -expand 0;
 
       update idletasks;
 
       set width  [winfo reqwidth $top.label];
       set height [winfo reqheight $top.label];
     }
 
     if {($state($w.text) != "") ||
         ($state($w.textvariable) != "")} {
       if {$state($w.textvariable) != ""} {
         upvar 0 $state($w.textvariable) textvariable;
 
         set state($w.text) $textvariable;
       }
       
       pack [message $top.text \
         -text       $state($w.text) \
         -background $state($w.background) \
         -foreground $state($w.foreground) \
         -font       {{San Serif} 10} \
         -aspect     10000 \
         -justify    left \
       ] -side top -fill x -expand 0;
 
       update idletasks;
 
       catch {
         if {$width < [winfo reqwidth $top.text]} {
           set width [winfo reqwidth $top.text];
         }
 
         incr height [winfo reqheight $top.text];
       }
     }
 
     catch {
       update idletasks;
 
       if {[winfo pointerx $w]+$width > [winfo screenwidth $w] && 0} { ;# no longer doing this, so works better on multi-monitor systems
         set x [expr {[winfo screenwidth $w] - 10 - $width}];
       } else {
         set x [expr {[winfo pointerx $w] + 10}];
       }
       
       wm geometry $top \
         ${width}x${height}+${x}+[expr {[winfo pointery $w]+10}];
       wm deiconify $top;
 
       raise $top;
 
       set state($w.id) [after \
         $state($w.dismissdelay) \
         [concat [namespace code destroyCB] $w] \
       ];
     }
 
     return;
   }
 
   namespace export -clear balloon;
 }



namespace export Tproc tla task_monitor tdump putz treturn wait tlg send_command tset tcall repos tgroup twait twidgets Task xwait comma tname tvwait tpause_check
}
# end of tasks namespace eval


