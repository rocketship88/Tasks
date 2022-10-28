# - --------------------------------
# C O N N F I G U R A T I O N begin
# - --------------------------------
set ::___zz___(proc_wid) 5          ;# the min number of lines to show BELOW (just changed) a breakpoint line, adjust with spinbox
set ::___zz___(auto_list_default) 1 ;# this sets the auto list checkbox to this value at first creation of checkbox
set ::___zz___(bp_messages_default) 1 ;# this sets the no bp messages checkbox to this value at first creation of checkbox
set ::___zz___(console_hack) 0      ;# if 1, installs a console hack to allow an empty <cr> line on console, repeats last command (handy for go+)
set ::___zz___(tooltips) 3000       ;# if > 0 tooltip enabled and value=delay, if the package require fails, it will report and work w/o it, 0=don't use
set ::___zz___(tooltipsbuiltin) 0   ;# if > 0 use hobb's tooltip package, at bottom of this file if no tooltips package available, e.g. on linux w/o tcllib
set ::___zz___(use_ttk) 0           ;# if 1, some windows use the themed ttk, but not the label or entries since we use -bg
set ::___zz___(max_size) 1000       ;# the maximum size of a variable, for safety, also if the variable does not yet exist, we can't monitor it
set ::___zz___(max_array_size) 500  ;# the maximum size of a array in indices
set ::___zz___(max_history) 50      ;# the maximum number of commands saved in the 2 command histories (command and uplevel)
set ::___zz___(max_frames) -20      ;# the maximum number nested procedure calls tracked, must be negative
set ::___zz___(skip_modulo) 100     ;# when using a large skip count on go+ this is the number of steps between reporting remaining messages
set ::___zz___(arrow) "\u27F6"      ;# Unicode arrow, can be 2 char positions also, can cause a wobble of the line number, if you like that
set ::___zz___(carrow) "\u2727"     ;# Unicode coverage arrow, or whatever it is 
set ::___zz___(tabsize) 4           ;# code window tabsize
set ::___zz___(fontsize) 12         ;# data window font size 
set ::___zz___(minupdate) 1         ;# this causes an update of only the arrow, otherwise redraws code on each step, can't be on to show instrumentation
set ::___zz___(deadman) 100         ;# when all bp's are off, we can appear to freeze if there's a lot of work to do, so every so often we update
set ::___zz___(deadman2) -1         ;# decr this guy until he reaches 0, then set to deadman, first thing we check in bp and lbp
set ::___zz___(procnolist) {tclLog history unknown pkg_mkIndex parray  tclPkgUnknown tclPkgSetup}  ;# list of proc's not to list for possible instrumentation, can lappend to this
set ::___zz___(procnolistre) {tcl_.*|tk_.*|auto_*|.*\+|\..*}  ;# a regular expr that if not empty will be used to filter the procs to instrument set

# choose one set or the other of the below, must be defined to something however
#set ::___zz___(black) black        ;# the code window colors, black is the foreground, white the background, yellow backgound when proc done
#set ::___zz___(white) white        ;#
#set ::___zz___(yellow) {#ffffc0}   ;# background when proc done
#set ::___zz___(yellowx) black      ;# foreground when proc done
 
set ::___zz___(black) {#ffffff}     ;# the code window colors, black is the foreground, whilte the background, yellow backgound when proc done
set ::___zz___(white) {#33393b}     ;# the bacground color from awdark theme
set ::___zz___(yellow) {#ffffc0}    ;# our shade of yellow 
set ::___zz___(yellowx) black       ;# but need to make text dark to read it 


#set ::___zz___(bwidget) 0 ;# uncomment this if BWidgets are not wanted, leave undefined and it will try to use it (do not set to 1 here)
catch {history keep 100}            ;# keep console history more than just 20, can comment this out, it's for debugging the debugger

#interp alias {} v {} vw+ ;# shorthands since we might be typing these, optional, now commented out to avoid colisions
#interp alias {} g {} go+
#interp alias {} u {} util+
#interp alias {} i {} instrument+

# - ------------------------------
# C O N N F I G U R A T I O N end
# - ------------------------------

#
#these statements are at global level, outside of all procs, and shouldn't be changed (unless you really know what you're doing)

set ::___zz___(lg-skip) [linsert [info global] end ___zz___] ;# skip initial system set of globals, + this one
set ::___zz___(skips) 0         ;# the number of breakpoints to skip, set here to avoid an info exist test, do not change, internal use only
set ::___zz___(showinstr) 0     ;# show instrumentation in code window
set ::___zz___(cb1) 0           ;# the global wide breakpoint disable flag, set it here so we don't have to check for existance later
set ::___zz___(level) 0         ;# 
set ::___zz___(delay) 0         ;# debugging delay times to slow down what's going on
set ::___zz___(goto) -1         ;# debugging goto line number
set ::___zz___(go-window) {}    ;# debugging goto window/proc name
set ::___zz___(lbp+,line) -1    ;# current line in a code window - 1
set ::___zz___(lbp+,pline) -1   ;# previous line in a code window - 1
set ::___zz___(lbp+,pproc) {}    ;# previous line proc name
set ::___zz___(tail) 0          ;# used for a tailcall to update lines w/o continuing
set ::___zz___(bpnum) 0
set ::___zz___(delaya) 0        ;# spinbox for delaying stepping animation
set ::___zz___(delayb) 1        ;# spinbox for changing precision, how many instructions per bp's animation
set ::___zz___(delayb_count) 1  ;# remaining instructions per bp's animation, but only if g values set, i.e. single step always just one
set ::___zz___(waita) 0         ;# variable to use for a vwait delay
set ::___zz___(trace-level) 0   ;# keep track of enter/leave so if we are in a lower level instrumented proc, we wait to turn it yellow on leave
set ::___zz___(queued_cmd) {}   ;# so we can do an uplevel
set ::___zz___(lbp-ontop)  0    ;# the code window on top checkbox
set ::___zz___(lbp-lock) {}     ;# used to lock the code window in place
set ::___zz___(updatesome)  10  ;# update at least once this many steps
set ::___zz___(updatesomeN)  0  ;# counter to use with updatesome
set ::___zz___(forcerefresh) 300  ;# to make it recompute too large variables, do this every N bp's
set ::___zz___(gangcb)       0  ;# checkbutton for gang moving of data windows, first window moves all together
set ::___zz___(gang)         {} ;# 
set ::___zz___(noflylist)    {} ;# list of bad variables we no longer can -textvariable with
set ::___zz___(cache)       1   ;# cache window label/entries - too slow on linux w/o this
set ::___zz___(coverage)    0   ;# don't erase arrows if 1, needs new minimal updates to work
#set ::___zz___(c,*)            ;# array data for window caching, do not modify

set ::___zz___(vw+) "vw+"       ;# the name of the vw+ proc (can perhaps change these if desired, both here and any aliases)
set ::___zz___(go+) "go+"       ;# the name of the go+ proc 
set ::___zz___(bp+) "bp+"       ;# the name of the bp+ proc
set ::___zz___(lbp+) "lbp+"     ;# the name of the lbp+ proc
set ::___zz___(util+) "util+"   ;# the name of the font adjuster, didn't want to use apply, would make the callback look too ugly



if { 0 } { ;# only for debugging
proc xputs {args} {
    
}
proc zwait {args} {
    lassign $args dly
    if { $::___zz___(delay) > 0 } {
        wait $::___zz___(delay)
        wait $dly
    }
    
}
proc wait { ms } {
    set uniq [incr ::__sleep__tmp__counter]
    set ::__sleep__tmp__$uniq 0
    after $ms set ::__sleep__tmp__$uniq 1
    vwait ::__sleep__tmp__$uniq
    unset ::__sleep__tmp__$uniq
}
proc timeus {args} {
    set result [uplevel 1 time $args]
    set number [format %.3f [expr {( [lindex $result 0] / 1 )}]]
    set number [regsub -all {\d(?=(\d{3})+($|\.))} $number {\0,}]
    return "$number microseconds [lrange $result 2 end]"
}
proc timems {args} {
    set result [uplevel 1 time $args]
    set number [format %.3f [expr {( [lindex $result 0] / 1000 )}]]
    set number [regsub -all {\d(?=(\d{3})+($|\.))} $number {\0,}]
    return "$number milliseconds [lrange $result 2 end]"
}
proc comma {num {sep ,}} {
    if { $num < 0 } {
        return "-[comma [expr {(0 - $num )}]]"
    } else {
        while {[regsub {^([-+]?\d+)(\d\d\d)} $num "\\1$sep\\2" num]} {}
    }
    return $num
}

}

proc parray {a {pattern *} {sort -dictionary} args} { ;# default dictionary sort  <<<< change
    upvar 1 $a array
    if {![array exists array]} {
        return -code error "\"$a\" isn't an array"
    }
    set maxl 0
    set names [lsort {*}$sort {*}$args [array names array $pattern]] ;# other sort options <<< change
    foreach name $names {
        if {[string length $name] > $maxl} {
            set maxl [string length $name]
        }
    }
    set maxl [expr {$maxl + [string length $a] + 2}]
    foreach name $names {
        set nameString [format %s(%s) $a $name]
        puts stdout [format "%-*s = %s" $maxl $nameString $array($name)]
    }
}

# note: several procs in this file have variable names, to allow for easier changing of their names
proc $::___zz___(vw+) {{pat {**}}  {w .vw} {wid 80} {alist {}}} {
    set w [string map {" " _} $w] ;# window names with spaces are a problem, change to _
    set me $::___zz___(vw+) ;# name of this procedure, used for recursion, callbacks, and help
    set go $::___zz___(go+) ;# name of the go from breakpoint command, used in a callback
    if { $pat eq "?" } {
        puts "$me  pattern   window   width   - patterns are \[string match\] type"
        puts "$me  {a list}  window   width   - alt form, list of >1 variable names or patterns"
        puts ""
        puts "   pattern   * => all globals   ** => only user globals (the default)"
        puts "             text  => text*  - can also use glob with \[abc\] etc. * added to end "
        puts "             foo:: => foo::* vars, not globals but namespace variables only"
        puts "   width     width of entry widget with variable data, defaults to 80"
        puts "   window    default to .vw, can use several windows at same time"
        puts ""
        puts "   {a list}  a list of specific variables or \[info global/vars\]"
        puts "             can be undefined (uses vars for namespaces)"
        puts ""
        puts "   Any parameter can be a . = shorthand for default, none are required"
        puts "   Note: when a single pattern, a * is added, but if a list, it's not"
        puts "         a pattern must be in {}'s to use the \[ab\] string pattern" 
        puts ""
        puts "   On first call to vw+, BWidget's is loaded if possible to support scrolling"
        puts "   if it's not available, will fall back to a single window; could be too large"
        return
    }
# ------------------------------------ vw + ---- modify console --------------------------------------------------------
    if { $::___zz___(console_hack) == 1} {
        set ::___zz___(console_hack) 2 ;# so we only do this once, first time through
        console eval {
            namespace eval tk { ; # replace this so we can capture a null command and repeat the last one
                proc ::tk::ConsoleInvoke {args} {
                    set ranges [.console tag ranges input]
                    set cmd ""
                    if {[llength $ranges]} {
                        set pos 0
                        while {[lindex $ranges $pos] ne ""} {
                            set start [lindex $ranges $pos]
                            set end [lindex $ranges [incr pos]]
                            append cmd [.console get $start $end]
                            incr pos
                        }
                    }
                    if {$cmd eq ""} {
                        ConsolePrompt
                    } elseif {[info complete $cmd]} {
                        if { $cmd == "\n" } { #patch
                            set cmd_next [consoleinterp eval {history nextid}]
                            set cmd_event [consoleinterp eval "history event [expr {( $cmd_next - 1 )}]"]
                            if { $cmd_event != "" } {
                                set cmd $cmd_event
                                consoleinterp eval {namespace eval ::tcl {incr history(nextid) -1;incr history(oldest) -1}}  ;# don't store this one again into history
                            }
                        }
                        #end patch
                        .console mark set output end
                        .console tag delete input
                        set result [consoleinterp record $cmd]
                        if {$result ne ""} {
                            puts $result
                        }
                        ConsoleHistory reset
                        ConsolePrompt
                    } else {
                        ConsolePrompt partial
                    }
                    .console yview -pickplace insert
                }
            }
        }
    }
# ---------------------------------------------- end modify console ----------------------------------------------------
    if { [llength $pat] > 1 } {
        return [$me . $w $wid $pat] ;# allow list to be in first parameter, we recurse with list at the end
    }
    set alist0 $alist ;# remember how we were called for later refresh
# ---------------------------------------------- Bwidget  --------------------------------------------------------------

    if { ! [info exist ::___zz___(bwidget)] } { ;# if variable does not exist, try to use it
        if [catch {
            package require BWidget
            set ::___zz___(bwidget) 1
        } err_code] {
            puts stderr "Turning off scrolling: $err_code"
            set ::___zz___(bwidget) 0
        }
    } else {
    }
# ---------------------------------------------- special entry from recursion  -----------------------------------------
    if { $pat eq "-" } { ;#refresh array entries index values, internal call via recursion
        set windows [winfo children $w]
        set k [string length $w]
        incr k
        foreach window $windows {
            set kind [string index $window $k]
            set wnum [string range $window $k+1 end]
            if { $kind eq "l" } { ;# this is the label on the left, we derive the entry on right
                set var [$window cget -text]
                if { [string range $var end-1 end] eq "()"} {
                    set avar [string range $var 0 end-2]
                    set ent "${w}.e$wnum" ;# the corresponding entry widget
                    set indices [lsort -dictionary [array names ::$avar]] ;# sort the array indices to put into entry
                    $ent configure -state normal  ;# make it writeable temporarily
                    $ent delete 0 end
                    $ent insert 0 $indices
                    $ent configure -state readonly  ;# and back to read only
                }
            }
        }
        return
    }
    
# ---------------------------------------------- setup . shortcuts  ----------------------------------------------------
    
    
    if { $pat eq "." } {
        set pat "**"
    }
    if { $wid eq "." } {
        set wid 80
    }
    if { $w eq "." } {
        set w ".vw"
    }
    set fsize $::___zz___(fontsize)
    array set fc { 14 .9 13  .95 12 1.0        11 1.1      10 1.2      9 1.3      8 1.4      } ;# pairs font size vs. width adjustment
    if { [info exist fc($fsize) ]} { ;# if fsize is smaller than 12, we need to adjust width
        set wid  [expr {    int(  $wid * $fc($fsize) )   }]
    }
    
    
    
    if { [string index $w 0] ne "." } {
        set w .$w ;# allow user to leave off the leading .
    }
    if { ![info exist ::___zz___(vws)] || $w ni  $::___zz___(vws)} {
        lappend ::___zz___(vws) $w ;# keep a list of possible windows to refresh
    }
# ---------------------------------------------- setup ** or list of vars ----------------------------------------------
    if { $pat eq "**" || $alist ne ""} {
        if { $alist ne "" } {
            set a {}
            foreach item $alist {
                if { [string match {*(*} $item] } { ;# if it's got ()'s  then just use it, it's not a pattern but a specific array element
                    lappend a $item
                } elseif { [string match {*::*} $item] } { ;# if it's got :: then use info var instead, not info glob
                    lappend a {*}[info vars $item]  
                } else {
                    lappend a {*}[info global $item]        
                }
                
            }
            
#           set a $alist ;# this was before we allowed the list to have patterns for each item
        } else {
            set a [lsort -dictionary [info global ${pat}*]]
        }
        set argsn [list]
        foreach gvar $a {
            if { $gvar in $::___zz___(lg-skip)  && $pat eq "**" && [llength $alist] == 0} { ;# no pattern given, use only user globals
                continue
            }
            if {[array exists ::$gvar]} { ;# it is an array get some indices only
                if { [array size ::$gvar] > $::___zz___(max_array_size) } {
                    set val "() too large: [array size ::$gvar] [lrange [lsort -dictionary  [array names ::$gvar] ] 0 100]"
                    lappend argsn [list $gvar $val]
                } else {
                set val "() [lsort -dictionary [array names ::$gvar]]"
                lappend argsn [list $gvar $val]
                }
            } elseif { [info exists ::${gvar}] } {
                lappend argsn [list $gvar {}]
            } else {
                lappend argsn [list $gvar {}] ;# variable doesn't exist yet, treat as non-array, only occurs with user provided list
            }
        }
# ---------------------------------------------- setup single pattern we append *  -------------------------------------
    } elseif { [llength $pat] == 1 } {
        if { [string match "*::*" $pat] } { ;# if we have :: then it's a namespace lookup
            if { [string range $pat end-2 end] ne "::"} {
                
#               set pat ${pat}:: ;# if there are any :: in it, we need to have :: following, if they are not there, we'll add them
            }
            set alist [lsort [info var ${pat}*]]
        } else {
            set alist [lsort [info glob ${pat}*]]
        }
        if { [llength $alist] >= 1 } {
            $me $pat $w $wid $alist ;# call ourselves with the new manual list provided, if at least 1
        } else {
            error "No globals match ${pat}*"
        }
        return
    } else {
        should-not-happen
    }
# ---------------------------------------------- does the window already exist  ----------------------------------------

    set ww .[lindex [split $w .] 1] ;# top level in case it .a.b.c we want just .a
    if { [info exist ::___zz___(vws,$ww)] } { ;# if this exists, then the window existed at some time
        set g [lindex $::___zz___(vws,$ww)  end]
        set gg [split $g +]
        set oldgeom +[lindex $gg 1]+[lindex $gg 2]
        set reincarnated 1 ;# later we'll use this oldgeom for the position, but use the new size that is computed
    } else {
        set reincarnated 0
    }

# now we try to keep most of the window, just delete all the variables 
# and rebuild them, faster than destroying toplevel and starting over completely

    set exists [expr {(   [info command $w] ne ""   )}]

# ---------------------------------------------- window not exists  setup bwidget or not but create the top level set $w ---------
    if { ! $exists } {  
        if { $::___zz___(bwidget) } {
    #       package require BWidget, this was done already, but this is the bwidget setup
            toplevel $w
            set sw  [ScrolledWindow $w.sw -relief sunken -borderwidth 2]
            set sff [ScrollableFrame $w.sw.f]
            $sw setwidget $sff
            bind $w <MouseWheel> "$w.sw.f yview scroll \[expr {-%D/60}\] units"
            pack $sw -fill both  -expand yes
            set w [$sff getframe]
        } else {
            toplevel $w
        }
        
# ---------------------------------------------- window does exist use w that came in possibly modified to be under scrollable frame bwidgets --------------------
    } else {
        if { $::___zz___(bwidget) } {
            set w $w.sw.f.frame
        } else {
            # w is ok if there's no bwidgets
        }
    }
    
    if { ! $exists } {  
# --------------------------------------*------- build DATA window  ----------------------------------------------------
        frame $w.f1 -relief groove  -padx 5
        frame $w.f2 -relief groove ;# -background green
        grid $w.f1 $w.f2
        if { $::___zz___(use_ttk) } {
            set ttk "ttk::"
            set borderwidth {}    ;# wish that ttk would ignore options from tk that are not supported, wrote a TIP for that
        } else {
            set ttk ""
            set borderwidth {-borderwidth 1 -pady 0}
        }
        
#       ${ttk}button      $w.f1.b1   -text "Refresh"    -command [list $me - $w 0] ;# refresh the arrays, variables no problem
        ${ttk}button      $w.f1.b1   -text "Refresh"    {*}$borderwidth                          ;# refresh the arrays, variables no problem
#       ${ttk}button      $w.f1.b1   -text "Refresh"    -command [list $me $pat $w $wid] ;# refresh the arrays by re-issue vw command new maybe
        set ww .[lindex [split $w .] 1]
        if { $pat ne "**" } {
            set cmd "$me {$pat} {$ww} {$wid} {}" ;# this will cause a re check of the variables based on a pattern
        } else {
            set cmd "$me {$pat} {$ww} {$wid} {$alist0}" ;# default pattern, so use the given list
        }
        bind  $w.f1.b1 <ButtonRelease-1>        $cmd                        ;# click on refresh will rebuild with more/less variables and update arrays
        bind  $w.f1.b1 <Alt-ButtonRelease-1>    [list puts "bind: $cmd"]    ;# alt output the binding only
        bind  $w.f1.b1 <Shift-ButtonRelease-1>  "$me - \{$w\} 0"            ;# shift click on refresh will only update arrays
        
        ${ttk}button      $w.f1.b2   -text "Go"         -command [list $go -1 $w]  {*}$borderwidth ;# go from breakpoint, remember the current window for refresh
        
        if { $reincarnated == 0 } {
            if { ! [info exist ::___zz___(cb1)] } {
                after 100 "set ::___zz___(cb1) 0" ;# global wide breakpoints disable, all windows use same, 1 check sets all checks
            }
            if { ! [info exist ::___zz___(cb2,$ww)] } { ;# window may pre-exist, keep value, this one is for the automatic code listing on stdout
                set ::___zz___(cb2,$ww)  $::___zz___(auto_list_default) ;# got to set it now, so we use it now
                after 100 "set {::___zz___(cb2,$ww)}  {$::___zz___(auto_list_default)}" ;#but again so the window manager has time to run
            }
            
            if { ! [info exist ::___zz___(cb3,$ww)] } { ;# window may pre-exist, keep value, this one is for the automatic code listing on stdout
                set ::___zz___(cb3,$ww)  0 ;# got to set it now, so we can use it below
                after 100 "set {::___zz___(cb3,$ww)} 0" ;# for local breakpoints, we include the window name so this is window specific breakpoint disabling
            }
            if { ! [info exist ::___zz___(cb4,$ww)] } { ;# window may pre-exist, 
                set ::___zz___(cb4,$ww)  $::___zz___(bp_messages_default) ;# got to set it now, so we can use it below
                after 100 "set {::___zz___(cb4,$ww)} $::___zz___(bp_messages_default)" ;# this disables the messages about stopping at a breakpoint, with user message, and then continuing
            }
            if { ! [info exist ::___zz___(cb5,$ww)] } { ;# window may pre-exist, 
                set ::___zz___(cb5,$ww)  0 ;# got to set it now, so we can use it below
                after 100 "set {::___zz___(cb5,$ww)} 0" ;# shows experimental instrumentation code
            }
            if { ! [info exist ::___zz___(cb6,$ww)] } { ;# window may pre-exist, 
                set ::___zz___(cb6,$ww)  0 ;# got to set it now, so we can use it below
                after 100 "set {::___zz___(cb6,$ww)} 0" ;# unused checkbox
            }
            if { ! [info exist ::___zz___(cb7,$ww)] } { ;# window may pre-exist, 
                set ::___zz___(cb6,$ww)  0 ;# got to set it now, so we can use it below
                after 100 "set {::___zz___(cb6,$ww)} 0" ;# unused checkbox
            }
        }
        # now that we set the checkbuttons variables before creating the checkbuttons, the wm timing seems to not be an issue any longer
        # before that sometimes the checkbuttons were not set with their correct values
        ${ttk}checkbutton $w.f2.cb1 -text "No (all) BPs   "     -variable ::___zz___(cb1)
        ${ttk}checkbutton $w.f2.cb2 -text "Auto-list    "       -variable ::___zz___(cb2,$ww)
        ${ttk}checkbutton $w.f2.cb3 -text "No local BPs "       -variable ::___zz___(cb3,$ww)
        ${ttk}checkbutton $w.f2.cb4 -text "No BP messages    "  -variable ::___zz___(cb4,$ww)
        ${ttk}checkbutton $w.f2.cb5 -text "spare"               -variable ::___zz___(cb5,$ww)
        ${ttk}button      $w.f2.bb6 -text "Exit"                -command exit  {*}$borderwidth
        if { ! [info exist ::___zz___(tooltipspr)] } {
            set ::___zz___(tooltipspr) 1 ;# so we don't do this again
            if [catch {
                package require tooltip
            } err_code] {
                set ::___zz___(tooltips) 0
                puts stderr "Package tooltips not found, will continue... Can use builtin see config at top"
                namespace eval tooltip {
                    proc tooltip {args} {}
                }

            }
        }
        set topguy [winfo toplevel $w]
        set tcmd "wm attributes {$topguy} -topmost \$::___zz___(cb6,$ww)"
        ${ttk}checkbutton $w.f2.cb6 -text "On Top       " -variable ::___zz___(cb6,$ww) -command $tcmd 
        set ::___zz___(cb7,$ww) 0
        ${ttk}checkbutton $w.f2.cb7 -text "Manual Geom" -variable ::___zz___(cb7,$ww) -command $tcmd 
        tooltip::tooltip $w.f1.b1  "Refreshes with new variable pattern\nalso used to update array indices\nalt-left-click for initial cmd"
        tooltip::tooltip $w.f2.cb1 "Turn off all breakpoints, this applies to all code\nwindows, this checkbox is shared by all code windows"
        tooltip::tooltip $w.f2.cb2 "Automatic code listing updates"
        tooltip::tooltip $w.f2.cb3 "Turn off all LOCAL breakpoints, this applies to the \nprocedure/method in the title bar"
        tooltip::tooltip $w.f2.cb4 "Turn off all breakpoint messages \nlocal to this proc/method"
        tooltip::tooltip $w.f2.cb5 "unused"
        tooltip::tooltip $w.f2.cb6 "Keep window on top"
        tooltip::tooltip $w.f2.cb7 "Turn off automatic window sizing and positioning"
        
#       checkbutton .f.c3 -text {top} -variable stayontop -command {ontop}
#       
#       proc ontop {} {
#           wm attributes . -topmost $::stayontop
#       }

        grid $w.f2.bb6 $w.f2.cb1  $w.f2.cb2  $w.f2.cb3 $w.f2.cb4  $w.f2.cb7  $w.f2.cb6  ;# options in column 2
        grid $w.f2.bb6 -padx 20 ;# extra space between exit on left (so is there with small window)
        grid $w.f1.b2 $w.f1.b1
# ---------------------------------------------- reuse window partially ------------------------------------------------

    } else {
#       vwait ::fff
        set children [winfo children $w]
#       if { $::___zz___(delay) > 100 } {
#       }
        set got1 1
        set the_ns ""
        set the_children {}
        foreach child $children {
            set kind [winfo class $child]
            if { $kind eq "Label" } {
                lappend the_children [$child cget -text]
            }
            if { $kind ne "Frame" } { ;# kill all the labels and entries, but take the canoli - uh I mean keep the buttons etc.
                set splitwidget [split $child .]
                if { $got1 } {
                    set got1 0
                    set the_ns [lindex $splitwidget 1]
                }
                if { $::___zz___(delay) > 0 } {
                    wait $::___zz___(delay)
                }
                vwdebug::do_destroy $kind $child
            }   
        }
#       vwait ::ffff
        # need to delete the namespace here - nope, we don't do it
#       namespace delete $the_ns
        set childrenx {}
        foreach item $argsn {
            lappend childrenx [lindex $item 0 0]    
        }
        

#       set cur_bind [bind $w.f1.b1 <1>] ;# current binding on the refresh button ***** this is NOT needed, just comment out now
#       bind $w.f1.b1 <1> {}             ;# remove binding so we can replace him (not add to him)
#       lset cur_bind 1 4 $childrenx
#       bind $w.f1.b1 <1> $cur_bind
        
#       vwait ::ffff
    }
# ---------------------------------------------- process argsn a list of variable names --------------------------------
    
    set size "{consolas} $::___zz___(fontsize)"
#   set size {TkFixedFont 12}
#   font
    set maxwid 16 ;# compute max variable length so groove fills, but set minimum first
    if { [llength $argsn] == 0 } {
        puts stderr "no matching variables found for pattern $pat"
    }
    
    foreach ii  $argsn {
        set i [lindex $ii 0]
        set j [lindex $ii 1]
        set len [string length $i]
        if { [string range $j 0 1] eq "()" } {
            incr len 2
        }
        if { $maxwid < $len } {
            set maxwid $len
        }
    }
# ---------------------------------------------- second pass on argsn, above was to compute max length of variable names -----------------------------------------
    
    set n 0
    
    if { $alist ne "" && $pat eq "**"} { ;# a specific list, don't sort use order given
        set argsn_sort_maybe $argsn
    } else {
        set argsn_sort_maybe [lsort -dictionary $argsn]
        
    }
    foreach ii $argsn_sort_maybe {
        if { $::___zz___(delay) > 0 } {
            wait $::___zz___(delay)
        }
        set i [lindex $ii 0]
        set j [lindex $ii 1]
        set zok 1
        set zerror ""
# ---------------------------------------------- variable is an array setup label/entry for type array -----------------

        if { [string range $j 0 1] eq "()" } {
            vwdebug::do_label 1 $w.l$n -width $maxwid  -text "${i}()" -anchor w  -font "$size" -bd 1 -relief groove
            
            vwdebug::do_entry 1  $w.e$n -width $wid -textvariable {} -bg white  -font "$size"
            $w.e$n insert end [lrange $j 1 end]
            $w.e$n  configure -state readonly
        } else {
# ---------------------------------------------- variable is NOT an array ----------------------------------------------
            
            set sizemax $::___zz___(max_size)
            set sanity -1
            set sanityd "???"
# ------------------------------------------------------------ regular variables can be too long, do a sanity check ----------------------------------------------
            
            if [catch {
                set it $i
                if { ! [string match "*::*" $it] } { ;# we need to access it as global from here, so add the ::
                    set it "::$it"
                }
                set sanity [string length [set $it]]
                if { $sanity > $sizemax } {
                    set splitup [split $it "::"]
                    set nspace  [lindex $splitup  2 ]
                    set nname  __$nspace
                    set fname "::${nspace}::${nname}" ;# check for our proc name, it's the namespace name used twice with extra __
                    set zok 0
                    set sanityd [string range [set $it] 0 $sizemax] ;# the most we will display
                    set zerror "Too large to safely monitor : $sanity  = $sanityd"
                    if { $it eq $fname } { ;# if it's ours, we'll be less cautious and allow for a longer string, since it's the entire proc code
                        if { $sanity < ($sizemax * 10) } {
                            set zok 1   ;# so if it's the same as $it it's the enemy who is us
                        }
                    }
                } else {
                    set zok 1   
                }
            } err_code] {
                set zerror  "$err_code"
                puts "zerror= |$zerror| "
                set zok 0
            }
# -------------------------------------------------------------labels/entry for variable types or arr(var) also not an array type --------------------------------
#       vwait ::ffff
            
            vwdebug::do_label 2 $w.l$n -width $maxwid -text $i -anchor w  -font "$size"  -bd 1  -relief groove
#       vwait ::ffff
            if { ! $zok || $i in $::___zz___(noflylist) } {
#               continue
                vwdebug::do_entry 2 $w.e$n  -width $wid -textvariable {} -bg LightYellow1 -font "$size" ;# no text variable for this one
                $w.e$n insert end $zerror
            } else {
                vwdebug::do_entry 3  $w.e$n -width $wid -textvariable $i -bg white  -font "$size"
            }
#           entry $w.e$n -textvariable $i -bg white -width $wid -font "$size"
        }

# ---------------------------------------------- is it zok, if it is then continue with bindings  ----------------------

        if { $zok } {
            vwdebug::do_bind  $w.l$n <3> {apply [list {win} {
                    set label [$win cget -text]
                    puts stderr "$label is on the no -textvariable list"
                    lappend ::___zz___(noflylist) $label
                    set foofoo [regsub {\.l} $win .e    ]                                                                                       
                    if [catch {
                        $foofoo config -textvariable {} -bg "light pink"
                    } err_code] {
                        puts stderr $err_code 
                    }
                } ] %W}
            vwdebug::do_bind  $w.e$n <3> {apply [list {win} {
                    puts stderr "RC'd entry window $win -textvariable is /[$win cget -textvariable]/ Right Click the label to disable"
                } ] %W}
            vwdebug::do_bind  $w.l$n <1> {apply [list {win} {
                    set foofoo [$win cget -text]
                    if { [string range $foofoo end-1 end] eq "()" } {
                        puts stderr "\n---------\nThe array $foofoo\n---------"
                        if [catch {
                            parray ::[string range $foofoo 0 end-2]
                        } err_code] {
                            puts $err_code 
                        }
                    } else {
                        puts stderr "\n------------\nThe variable $foofoo\n------------"
                        puts "$foofoo = |[set ::$foofoo]|"
                    }
                } ] %W}
            vwdebug::do_bind  $w.l$n <Shift-1> {apply [list {win} {
                    set foofoo [$win cget -text]
                    if { [string range $foofoo end-1 end] eq "()" } {
                        puts stderr "\n---------\nThe array $foofoo\n---------"
                        parray ::[string range $foofoo 0 end-2]
                    } else {
                        puts stderr "\n--------------\nThe Dictionary $foofoo\n--------------"
                        set llen [llength [set ::$foofoo]]
                        set ok 0
                        if [catch {
                            set foo2 [set ::$foofoo]
                            set dlen [dict size $foo2]
                            incr dlen $dlen
                            if { $dlen != $llen } {
                                error "not a valid dictionary:\nAre there duplicates? \[llength\]= $llen vs. \[dict size\]*2 = $dlen"
                                set ok 0
                            } else {
                                set ok 1
                            }
                        } err_code] {
                            puts "dictionary error: $err_code"
                            set ok 0
                        }
                        if { $ok } {
                            set max 0
                            dict for {key val} $foo2 {
                                set len [string length $key] 
                                if { $len > $max } {
                                    set max $len
                                }
                            }
                            set fstring "  %%-${max}s  =>  |%%s|" 
                            dict for {key val} $foo2 {
                                puts [format $fstring $key $val]
                            }
                        } else {
                        }
                        
                    }
                } ] %W}
            vwdebug::do_bind  $w.l$n <Control-1> {apply [list {win} {
                    set foofoo [$win cget -text]
                    if { [string range $foofoo end-1 end] eq "()" } {
                        puts stderr "\n---------\nThe array $foofoo\n---------"
                        parray ::[string range $foofoo 0 end-2]
                    } else {
                        set llen [llength [set ::$foofoo]]
                        puts stderr "\n--------\nThe List $foofoo   llength: $llen\n--------"
                        set ok 1
                        if [catch {
                            set foo2 [set ::$foofoo]
                        } err_code] {
                            puts "list error: $err_code"
                            set ok 0
                        }
                        if { $ok } {
                            set n -1
                            set fstring "  %%-4d  =>  |%%s|" 
                            foreach val $foo2 { ;# don't sort here
                                puts [format $fstring [incr n] $val]    
                            }
                        } else {
                        }
                    }
                } ] %W}
            vwdebug::do_bind  $w.l$n <Alt-1> {apply [list {win} {
                    set foofoo [$win cget -text]
                    if { [string range $foofoo end-1 end] eq "()" } {
                        puts stderr "\n---------\nThe array $foofoo\n---------"
                        parray ::[string range $foofoo 0 end-2]
                    } else {
                        set llen [llength [set ::$foofoo]]
                        puts stderr "\n--------\nThe List $foofoo  (sorted -dictionary) llength: $llen\n--------"
                        set ok 1
                        if [catch {
                            set foo2 [set ::$foofoo]
                        } err_code] {
                            puts "list error: $err_code"
                            set ok 0
                        }
                        if { $ok } {
                            set n -1
                            set fstring "  %%-4d  =>  |%%s|" 
                            foreach val [lsort -dictionary $foo2] { ;# alt is for a sorted list
                                puts [format $fstring [incr n] $val]    
                            }
                        } else {
                        }
                    }
                } ] %W}
        }
# ---------------------------------------------- end zok test for valid data put the variable and it's entry in the 2 column grid --------------------------------
            
            
        vwdebug::do_grid  $w.l$n $w.e$n
        incr n
    }
# ---------------------------------------------- end argsn pass 2 ------------------------------------------------------
    
    update ;# wonder if this is needed here
# ---------------------------------------------- no bwidgets easy let window size itself even if too large just reuse old geom -----------------------------------
    if {! $::___zz___(bwidget)} {
        set ww [lindex [split [wm geom $w] +] 0]
        if { $reincarnated } {
            set newgeom $oldgeom
        } else {
            set newgeom +-6+1
        }
# ---------------------------------------------- modify GEOMETRY no bwidgets -------------------------------------------
        set ww0 .[lindex [split $w .] 1]
        if {[info exist ::___zz___(cb7,$ww0)] && $::___zz___(cb7,$ww0) == 0} {
                wm geom $w $ww$newgeom 
        }
# ---------------------------------------------- bwidgets try to compute new size of window ----------------------------
    } else {
#       resize the toplevel window so to not need scrollbars if possible, but no bigger than these maxes
        set fheight [font metrics "consolas $::___zz___(fontsize)" -linespace] 
        set fwidth [font measure  "consolas $::___zz___(fontsize)" "X"]
        
        set height1     [expr {(    min(  int( 65 + ($n * 1.2 * $fheight) ) , 950 )                    )}] ;# no more than 950
        set width1      [expr {(    min( int( ($wid + $maxwid) * 1.03 * $fwidth ) +12 , 1600)    )}] ;# no more than 1600
        
#       set height  [expr {(    min(  65 + ($n * 27) , 950 )                    )}] ;# no more than 950
#       set width   [expr {(    min( int( ($wid + $maxwid) * 11.4) +12 , 1600)    )}] ;# no more than 1600
        if { $reincarnated } {
#           set newgeom ${width}x$height$oldgeom
            set newgeom ${width1}x$height1$oldgeom
        } else {
            if [catch {
                set nws [llength $::___zz___(vws) ]
                incr nws -1
            } err_code] {
                set nws 0 
            }
            set nws [expr {(    $nws % 10   )}] ;# after this many new windows, we start placing them at the top again
            set ycord [expr {(   -6 + ($nws * 100)   )}]
            set newgeom ${width1}x$height1+-6+$ycord
        }
        set top [split $w .]
        set wtop .[lindex $top 1]
        if { $::___zz___(skips) <=0 || $reincarnated == 0} {
            if [catch {
            } err_code] {
                puts $err_code 
            }
# ---------------------------------------------- modify GEOMETRY bwidgets ----------------------------------------------
            set ww0 .[lindex [split $w .] 1]
            if {[info exist ::___zz___(cb7,$ww0)] && $::___zz___(cb7,$ww0) == 0} {
                catch {wm geom $wtop $newgeom}
            }
        }
    }

# ---------------------------------------------- setup user configure callback to store the saved geometry of his new position -----------------------------------

    if [catch {
        set wl [split $w .]
        set ww .[lindex $wl 1]
        set ::___zz___(vws,$ww) [list $ww $pat $wid $alist [wm geom $ww]]
#       set ::___zz___(vws,$ww) [list $ww $pat $wid {} [wm geom $ww]]
        bind $ww <Configure> {
            if { [llength [split %W .]] == 2 } {
                lset ::___zz___(vws,%W) end [wm geom %W] ;# update to the current position and size
            } else {
            }
        }
    } err_code] {
        puts " Cannot set the vws,$w : $err_code "
    }
    
    return
#   flush stdout ; update
} ;# addapted from the original idea by RS
#$::___zz___(bp+)


# ----------------------------------- bp + ----- low level breakpoint --------------------------------------------------


proc bp+ {{message {*}}  {nobreak 0}  {nomessage 0} {nocount 0}} { ;# the 2nd, 3rd, passed in from lbp+ from the windows checkbox options
    if { [incr ::___zz___(deadman2) -1]  < 0} {
        update ;# to avoid freezing totally
        set  ::___zz___(deadman2) $::___zz___(deadman)
    }
# ---------------------------------------------- spinbox delay setting -------------------------------------------------

    if { $::___zz___(delaya) > 0 } {
        set ::___zz___(waita) 0
        if [catch {
                    after $::___zz___(delaya) {set ::___zz___(waita) 1}
                vwait ::___zz___(waita)
        } err_code] {
            puts stderr "probably bad delay, resetting to 0 : $err_code" 
            set ::___zz___(delaya) 0
        }
    }

# ---------------------------------------------- try to escape early ---------------------------------------------------
    if { $::___zz___(level) > 0} {
        if { [incr  ::___zz___(level_message_count) ] > 10  } {
            return
        }
        puts stderr "no recursive breakpoints allowed, ignoring, level = $::___zz___(level) / $::___zz___(level_message_count) "
        return
    }
    set stophere 0
    if { $::___zz___(goto) >= 0 } {
        if { [expr {(   $::___zz___(lbp+,line) + 1    )}] ==  $::___zz___(goto) } {
            if {  [lindex [vwdebug::get_frames] 0 ]  eq $::___zz___(go-window)} {
                set stophere 1  
                set ::___zz___(goto) -1 
            } else {
            }
        }
    } else {
        set stophere 1
    }
    if { $::___zz___(cb1) || $nobreak} {
        if {! $nomessage } {
            puts stderr "AlertPoint   [expr {(   $::___zz___(bpnum) + 1   )}]  :  $message"
        }
        incr ::___zz___(bpnum)
        return
    }
    if { ![info exist ::___zz___(vws)] } {
        set ::___zz___(vws) [list]
    }
# ---------------------------------------------- report every so often about skips if reporting at all and get out early -----------------------------------------

    if {   ($::___zz___(skips)  <= 0)   ||  (  ( $::___zz___(skips)% $::___zz___(skip_modulo) ) == 0    )       } {
    } else {
        incr ::___zz___(skips) -1
        if { ! $nomessage } {
            if { $::___zz___(skips) > 0 } {
                set taco "skips remaining : $::___zz___(skips)"
            } else {
                set taco ""
            }
            puts stderr "BReakpoint   [expr {(   $::___zz___(bpnum) + 1   )}]  :  $message"
            update
        }
        incr ::___zz___(bpnum)
        return
    }
# ---------------------------------------------- low level breakpoint try to refresh windows----------------------------
    foreach vwindow $::___zz___(vws) {
        if { [info command $vwindow] ne "" } { 
            if { [info exist ::___zz___(bwidget)] && $::___zz___(bwidget) == 1} {
                set bound [bind $vwindow.sw.f.frame.f1.b1 <Shift-1>]
#               catch {eval $bound}
#               catch {$vwindow.sw.f.frame.f1.b1 invoke} ;# we've removed the -command and use only the bindings, so can't invoke now
            } else {
                set bound [bind $vwindow.f1.b1 <Shift-1>]
#               catch {eval $bound}
#               catch {$vwindow.f1.b1 invoke}
            }
        } else {
            puts "delete vwindow= |$vwindow| "
            set n [lsearch -exact $::___zz___(vws) $vwindow] ;# it has to be there, but we don't know where if some were deleted
            set ::___zz___(vws) [lreplace $::___zz___(vws) $n $n]
        }   
    }

# ---------------------------------------------- low report breakpoint reporting ---------------------------------------
    
    
    if { ! $nomessage } { #; even though we might not actually breakpoint here, we might still send a message, works as a trace
        if { $::___zz___(skips) > 0 } {
            set taco "skips remaining : $::___zz___(skips)"
        } else {
            set taco "" 
        }
        puts stderr "Breakpoint  [incr ::___zz___(bpnum)] :  $message $taco "
        update 
#       flush stdout ; update ;# needed here?
    } else {
        if { $nocount == 0 } {
            incr ::___zz___(bpnum)
        }
    }
    if { $nomessage } {
        set ::___zz___(bp) 2 ;# indicate we are waiting for it to change but don't want
    } else {
        set ::___zz___(bp) 0 ;# indicate we are waiting for it to change
    }
    if { $::___zz___(skips) > 0 } {
        incr ::___zz___(skips) -1
        set nobreak 1
    }

# ---------------------------------------------- check for a queued command from a callback to implement the uplevel commands ------------------------------------
# ---------------------------------------------- use a while loop to execute this 1 or 2 times, if we woke from callback do uplevel ------------------------------
    
    if { ! $nobreak && $stophere} {
        incr ::___zz___(level)
        while {1 } {
# ---------------------------------------------- ----- the vwait on the breakpoint -------------------------------------
            vwait ::___zz___(bp) ;# pause until this is set again
            if { $::___zz___(bp) < 100 } { ;# our internal value, when we do the uplevel command from the code window, 
                break                       ;# we can't do it from there, we need to do it from here, so we queue up the command and resume with 100 in (bp)
            }
            if { $::___zz___(bp) == 100 } { ;# do uplevel entry commands
                if [catch {
                        set ok 0
                        for {set m -1} {$m > -4} {incr m -1} {
                            set up      [uplevel [expr {(   0-$m   )}] info frame $m]
                            set vars    [uplevel [expr {(   0-$m   )}] info vars]
                            if { [dict exists $up "proc" ] } {
                                set prc [dict get $up "proc"]
                                if { $prc ne "::$::___zz___(lbp+)" && $prc ne "::$::___zz___(bp+)"} {
                                    set ok [expr {(   abs($m + 1)   )}]
                                    break
                                }
                            } elseif { [dict exists $up "method" ] } {
                                set prc [dict get $up "method"]
                                if { $prc ne "::$::___zz___(lbp+)" && $prc ne "::$::___zz___(bp+)"} {
                                    set ok [expr {(   abs($m + 1)   )}]
                                    break
                                }
                            }
                        }
                        if [catch {
                            set ok2 [uplevel $ok info vars]   ;# $::___zz___(queued_cmd)
                            puts stderr "result from uplevel for: \u3010 $::___zz___(queued_cmd) \u3011 :" 
                            set ok2 [uplevel $ok $::___zz___(queued_cmd)]   ;# alright do it in his level
                            puts "\u3010$ok2\u3011"
                        } err_code] {
                            puts $err_code
                        }
                } err_code] {
                        puts $err_code 
                }
            } elseif {$::___zz___(bp) == 101 || $::___zz___(bp) == 102 } { ;#list frames (102, just the cmd entries)
                        if { $::___zz___(bp) == 102  } {
                            puts "------------- -- ---------------"
                        }
                        for {set m $::___zz___(max_frames)} {$m <= 0} {incr m} {
                            if [catch {
                                set up      [ info frame $m]
#                               set vars    [uplevel [expr {(   0-$m   )}] info vars]
                                if { [dict exists $up proc] } {
                                    set p [dict get $up proc] 
                                    if {$p eq "::$::___zz___(bp+)" || $p eq "::$::___zz___(lbp+)" } {
                                        continue
                                    }
                                }
                                if { $::___zz___(bp) == 101 } {
                                    puts "------------- $m ---------------"
                                }
                                foreach item [dict keys $up] {
                                    set val [dict get $up $item]
                                    if { $item eq "level" } {
                                        continue
                                    }
                                    if { $item eq "cmd" && [string range $val 0 3]  eq $::___zz___(lbp+) } {
                                        continue
                                    }   
                                    set val2 [string range [string trim [string map {\n \u2936} $val]]  0 100] 
                                    if {  $item eq "cmd" } {
                                        if { [string length $val] > 100 } {
                                            set val2 "${val2} ..."
                                        }
                                        puts -nonewline stderr [format {    %-6s} $item]
                                        puts stderr [format { => %s} $val2]
                                        continue
                                    }
                                    if { $::___zz___(bp) == 101 } {
                                        puts [format {    %-6s => %s} $item $val2]
                                    }
                                }
                                
                            } err_code] {
                                if { $err_code  ne ""} {
                                }
                                
                            }
                        }
            }
        }
        incr ::___zz___(level) -1
    }
# ---------------------------------------------- done and about to continue --------------------------------------------
    
    if { ! $nomessage &&  ! $nobreak} { ;# if we didn't pause, then we don't say continue
        puts stderr "Continuing..."
    }
    set  ::___zz___(bp) 1
}

# ----------------------------------- go + ----- go command  -----------------------------------------------------------


proc $::___zz___(go+) {{skip -1} {window ""}} {
    if { $skip < -1 } {
        set ::___zz___(goto) [expr {(   0 - $skip   )}]
        set skip -1
        set curr [lindex [vwdebug::get_frames] 0 ]
        set ::___zz___(go-window) $curr ;# this will be the target proc of a go to line
    }
    if { $skip > 0 } {
        set ::___zz___(skips) [expr {(   $skip - 1   )}]
    }
    if {![info exists ::___zz___(bp)] ||  ($::___zz___(bp) == 1) } {
        puts stderr "Not currently waiting at a breakpoint after $::___zz___(bpnum) steps"
        set ::___zz___(bp) 1 ;# set it regardless
    } elseif {![info exists ::___zz___(bp)] ||  ($::___zz___(bp) == 2)} {
        set ::___zz___(bp) 2 ;# set it so we continue
        return ""
    }   
    set ::___zz___(bp) 1 ;# set it regardless
    return ""
}


# ----------------------------------- util + --- utility command ensemble ----------------------------------------------

if { 00 } {
# trace add variable ::___zz___(bpnum) write "dtracer "
proc dtracer {args} {
    puts "dtracer -- args = $args"
    set frame [info frame]
    puts "frame= |$frame| "
    for {set n $frame} {$n > 0} {incr n -1} {
        if [catch {
            set nframe [info frame $n]
            puts "$n ---   nframe= |$nframe| "
        } err_code] {
            puts $err_code
        }
    }
    if { [info exist ::foobar] } {
        error "here is an error"
    }
    
}
}


proc $::___zz___(util+) {func args} { ;# increase or decrease font, and do the list proc as sub commands, plus many more now

# ------------------------------------------------------ utility fontsize ----------------------------------------------

    if       { $func eq "fontsize" } {
        lassign $args w dir
        lassign [$w cget -font] font size
        if { $dir > 0 } {
            incr size
            if { $size > 25 } {
                set size 25
            }
        } else {
            incr size -1
            if { $size < 6 } {
                set size 6
            }
        }
        set tfont "$font $size"
        $w config -font "$font $size" -tabs "[expr {$::___zz___(tabsize) * [font measure $tfont 0]}] left"
    } elseif { $func eq "init_method" } {           ;# clean up old method data
        lassign $args thens vars
        set cur [info vars ::${thens}::*]
        set widgets [winfo children .$thens.sw.f.frame]
        
        
        if [catch {
            set got1 1
            set the_ns ""
            set the_children {}
        
            
            set children [winfo children .${thens}.sw.f.frame]
            foreach child $children {
                set kind [winfo class $child]
                if { $kind eq "Label" } {
                    lappend the_children [$child cget -text]
                }
                if { $kind ne "Frame" } { ;# kill all the labels and entries, but take the canoli - uh I mean keep the buttons etc.
                    set splitwidget [split $child .]
                    if { $got1 } {
                        set got1 0
                        set the_ns [lindex $splitwidget 1]
                    }
                    if { $::___zz___(delay) > 0 } {
                        wait $::___zz___(delay)
                    }
                    vwdebug::do_destroy $kind $child
                }   
            }
        } err_code] {
            puts "error inside util: $err_code "
        }
        
        
        
        namespace delete ::$thens
#       foreach item $vars {
#           namespace eval ::$thens  "variable $item"   
#       }
        return ""
    } elseif { $func eq "refresh" } {           ;# refresh arrays on windows
        set window [lindex $args 0 ]
        if [catch {
            vw+ - .${window}.sw.f.frame 0 {}
        } err_code] {
            return $err_code 
        }
        return "ok"
    } elseif { $func eq "tabsize" } {           ;# see if the current line is a complete command, if not, find the end on following lines
        set ::___zz___(tabsize) [lindex $args 0 ]
        $::___zz___(util+) fontsize .lbp_console.cframe.text +1 ;# bigger/smaller to adjust
        $::___zz___(util+) fontsize .lbp_console.cframe.text -1
    } elseif { $func eq "completeit" } {    ;# see if the current line is a complete command, if not, find the end on following lines
        set nlines [lindex $args 0 ]
        set ln [lindex $args 1 ]
        incr ln -1
        set lines [lindex $args 2 ]
        set collect {}
        set nn 0
        for {set n $ln} {$n < $nlines } {incr n} {
            append collect [lindex $lines $n]\n
            incr nn
            if { [info complete $collect] } {
                break
            }   
        }
        return [list $nn $collect] ;# return the number of lines following to not instrument
# ------------------------------------------------------ showlines    --------------------------------------------------

    } elseif { $func eq "showlines" } {     ;# number of lines to show, is now immediate if one changes the spinbox for lines
        set ::___zz___(minupdate) 0
        set ::___zz___(tail) 1
        after 0 $::___zz___(go+)
        return
# ------------------------------------------------------ utility grid --------------------------------------------------

    } elseif { $func eq "gridck" } {    ;# turn on/off gang move
        if { $::___zz___(gangcb) } {
            set newws {}
            foreach w $::___zz___(vws) {
                if { [info command $w] eq "" } {
                    continue
                }
                lappend newws $w
                bind $w <Configure> {} ;# unbind if it was bound
            }
            set ::___zz___(vws) $newws
            if { [llength $newws] > 0 } {
                set w [lindex $newws 0 ]
                set ::___zz___(gang) [wm geom $w] ;# save his current position and bind him
                bind $w <Configure> [list $::___zz___(util+) gang-move %x %y]
                puts stderr "Grid leader is now: $w"
            } else {
                puts stderr "No windows to grid together"
                set ::___zz___(gangcb) 0
            }
        } else {
            
        }
        return
        
    
    
    } elseif { $func eq "grid" } {  ;# line up all the windows

        set x -9
        set y -2
        set max 1080
        set ws $::___zz___(vws)
        set index 0
        set wn [llength $ws]
        set extra 25
        set xincr 700
        if { $::tcl_platform(os) eq "Linux" } {
            set extra 50
        }
        if { [info exist ::___zz___(bwidget)] &&  $::___zz___(bwidget) == 0} {
            incr extra 25
        }
        if { [llength $args] > 0 } {
            set extra [lindex $args 0 ]
        }
        if { [llength $args] > 1 } {
            set xincr [lindex $args 1 ]
        }
        set ::___zz___(vws) {} ;# rebuild from our copy, less any that are deleted
        for {set n 0} {$n < $wn } {incr n} {
            set w [lindex $ws $n]
            if { [info command $w] eq "" } {
                continue
            }
            lappend ::___zz___(vws) $w
            set geom [wm geom $w]
            set xy [split $geom +]
            set xandy [split [lindex $xy 0 ] x]
            set xx [lindex $xy 1]
            set yy [lindex $xy 2]
            set newgeom [lindex $xy 0 ]+$x+$y
            set y [expr {   $y + [lindex $xandy  1 ] +$extra  }]
            if { $y > ($max - 300) } {
                set y -2
                incr x $xincr
            }
            wm geom $w $newgeom
            update
            raise $w
            after 50
            update
        }
        if { [llength $::___zz___(vws)] > 0 } {
            set w [lindex $::___zz___(vws) 0] ;# front of list is the new guy to lead
            set ::___zz___(gang) [wm geom $w] ;# save his current position and bind him
            bind $w <Configure> [list $::___zz___(util+) gang-move %x %y]
#           set ::___zz___(gangcb) 1
            puts stderr "Grid locking is off, use \"move group\" to turn on"
        } else {
            puts stderr "No windows to arrange"
            
        }
        set ::___zz___(gangcb) 0
        
        return
# ------------------------------------------------------ gang move callback   ------------------------------------------        
    } elseif { $func eq "gang-move" } {     ;# display all frames verbose
        set geom $::___zz___(gang)
        regexp {^([0-9]+)x([0-9]+)([+-])([+-]?[0-9]+)([+-])([+-]?[0-9]+)} $geom -> dx dy xs xpos ys ypos
        if { [llength $::___zz___(vws)] > 0 } {
#           update
            set geomnow  [wm geom [lindex $::___zz___(vws) 0]] ;# get the leader's current position
            regexp {^([0-9]+)x([0-9]+)([+-])([+-]?[0-9]+)([+-])([+-]?[0-9]+)} $geomnow -> dx dy xs xx ys yy ;# only need the xx and yy  
        } else {
            puts stderr "should never happen, no windows in vws"
            return
        }
        if { $xpos == $xx && $ypos == $yy } {
        } else {
            set dx   [expr {  $xx - $xpos   }]
            set dy   [expr {  $yy - $ypos   }]  
            set ::___zz___(gang) [wm geom [lindex $::___zz___(vws) 0]]
            foreach w [lrange $::___zz___(vws) 1 end] {
                set geom [wm geom $w]
                regexp {^([0-9]+)x([0-9]+)([+-])([+-]?[0-9]+)([+-])([+-]?[0-9]+)} $geom -> sxx syy xs xpos ys ypos
                set newx [expr {    $xpos + $dx   }]
                set newy [expr {    $ypos + $dy   }]
                set newgeom ${sxx}x${syy}+$newx+$newy
                if { $::___zz___(gangcb) } {
                    wm geom $w $newgeom
#                   wait 1000
                    update
                }
            }
        }
        return
# ------------------------------------------------------ utility frames-callback ----------------------------------------

    } elseif { $func eq "frames-callback" } {   ;# display all frames verbose
#       set ::___zz___(queued_cmd) {puts frames-callback} 
        set ::___zz___(bp) 101 ;# needs to be the last thing we do before we get outa here
        return
    } elseif { $func eq "frames-callback2" } {  ;# display just cmd entry in frames
        set ::___zz___(bp) 102 ;# needs to be the last thing we do before we get outa here
        return
    } elseif { $func eq "no-bp-messages-all" } {    ;# set all the checkboxes
        foreach item [array names ::___zz___ "cb4,*" ] {
            set ::___zz___($item) 1
        }
        return
# ------------------------------------------------------ utility enter-callback ----------------------------------------

    } elseif { $func eq "enter-callback" } {    ;# the 2 entry widgets and their callbacks, 3 args (in args)
    
        set n [lindex $args 0 ] ;# get the 3 arguments this ensemble has, n=1 or 2 for which entry box
        set w [lindex $args 1 ] ;# the window for the entry (2 of them)
        set key [lindex $args 2 ] ;# which key was typed, we handle enter (do the command) up/down for history
        if { [string is integer $key] } { ;# to use mousewheel
            if { $key < 0 } {
                set key Down
            } else {
                set key Up  
            }
        }
    

if { 1 } { ;# this is from the old debugger code, now in an ensemble instead of it's own (ugly) command


#.. proc ::___bp4g {n w key}  ;# callbacks for the entry widgets

    set var [$w cget -textvariable] ;# name of the variable, not the value
    set max $::___zz___(max_history)
#   ::___zz___(hnum,$n)     number 0..n for which one is next in list 0 = first
#   ::___zz___(history,$n)  history list
    if { ! [info exist ::___zz___(history,$n)] } {
        set ::___zz___(history,$n) [list]
        set ::___zz___(hnum,$n) -1
    }
    set queue_it 0
    if       { $key eq "Return" || $key eq "KP_Enter"} {
        set val [set $var] ;# get the actual value
        if { $val eq "" } {
            set lastone [lindex $::___zz___(history,$n) 0 ] 
            if { $lastone eq "" } {
                return
            }
            set val $lastone ;# use the last one
#           vwait forever
        }
        if { $n == 1 } { ;# which entry, 1 or 2, 1= do {...} 1
            set ::___zz___(queued_cmd) $val ;# we can't do it from the callback of the uplevel entry widget, only after the vwait in bp+
            set queue_it 1 ;# at the end, we'll set the vwait'd var to 100 so bp+ knows it's us
#           eval  "do \{$val\} 1"
        } else {
            if { [lindex $val 0] eq "g"  } {
                after 0 $val ;# if it's our g command, don't echo it to stderr/stdout
            } else {
#               after 0  "puts stderr \"result for \u3010 $val \u3011: \";puts \[$val\]" ;# make it run at global level, like the console
                
                puts  "result for \u3010 $val \u3011 "
                if [catch {
                    set xxx [uplevel #0 $val]
                } err_code] {
                    puts stderr "error: $err_code" 
                    return
                }
                puts "\u3010$xxx\u3011"
                
            }
        }
        $w delete 0 end ;# after doing it, we clear it out and reset hnum
        set ::___zz___(hnum,$n) -1
        if { $val ne  [lindex $::___zz___(history,$n) 0 ]   } {
            
            set ::___zz___(history,$n) [linsert $::___zz___(history,$n) 0 $val]
            if { [llength $::___zz___(history,$n)] > $max } {
                set ::___zz___(history,$n) [lrange $::___zz___(history,$n) 0 end-1]
            }
         }
#       la ::___zz___
    } elseif {  $key eq "Up"  } {
        if { [llength $::___zz___(history,$n) ] <= 0 } {
            return
        }
        set num $::___zz___(hnum,$n)
        incr num
        if { $num < [llength $::___zz___(history,$n)]} {
        } else {
            return
        }
        $w delete 0 end
        set val [lindex $::___zz___(history,$n) $num ]
        
        $w insert 0 $val
        incr ::___zz___(hnum,$n)

    } elseif { $key eq "Down"  } {
        if { [llength $::___zz___(history,$n) ] <= 0 } {
            return
        }
        set num $::___zz___(hnum,$n)
        incr num -1
        if { $num < [llength $::___zz___(history,$n)] && $num >= 0} {
        } else {
            $w delete 0 end ;# clear it out since there's no more, the next up will restore it
            incr ::___zz___(hnum,$n) -1
            if { $::___zz___(hnum,$n)  < 0} {
                set ::___zz___(hnum,$n) -1
            }
            return
        }
        $w delete 0 end
        set val [lindex $::___zz___(history,$n) $num ]
        
        $w insert 0 $val
        incr ::___zz___(hnum,$n) -1
        
    } else {
        
    }
    after 0 [list focus -force $w] ;#  make him active
    if { $queue_it } {
        set ::___zz___(bp) 100 ;# needs to be the last thing we do before we get outa here
    }
    return


}
# ------------------------------------------------------ utility double-click a line number  ---------------------------

    
    } elseif { $func eq "double-click" } {  ;# this is used to set the go -N value, to run till line number
        set selranges [.lbp_console.cframe.text tag ranges sel]
        set selection [.lbp_console.cframe.text get {*}$selranges]
        if { [string is integer $selection] } {
            tailcall $::___zz___(go+) "-[expr {(   abs($selection)   )}]"
            return
        } 
        puts stderr "Invalid double click selecton, not a number: $selection"
        return


# ------------------------------------------------------ reach end of a proc by trace callback  ------------------------

    
    } elseif { $func eq "tracerend" } {     ;# this is used at the end of a proc, to insert a unicode return char but leaving data window for final inspection
        set prc [lindex [vwdebug::get_frames] 0 ]                                                                                                               
        after 100 [list catch "wm title .lbp_console $prc"]
        incr ::___zz___(trace-level) -1
        if { $::___zz___(trace-level)  > 0} {
            return
        }
        if { $::___zz___(trace-level)  < 0} {
            set ::___zz___(trace-level) 0
        }
        if [catch {
            set last [lindex [split [.lbp_console.cframe.text index end] .] 0]
            if { !$::___zz___(cb1) && !$::___zz___(cb3) } {
                for {set n 1} {$n < ($last-1) } {incr n } {
                    .lbp_console.cframe.text replace ${n}.0 ${n}.0 "\u2936"
                }
#               .lbp_console.cframe.text configure -bg $::___zz___(yellow) -fg $::___zz___(yellowx)
            }
        } err_code] {
        }
        return

# ------------------------------------------------------  entry trace, start off a proc, restore window background from yellow if needed  ------------------------


    } elseif { $func eq "tracer" } {    ;# this is used to clear the namespace for the proc, 
                                        ;# clearing vars so next time in we start over, internal call only
# no longer setting bg color to indicate procedure has ended, since we can't figure out how to trace methods
# so we now just insert the unicode char for enter/return to indicate we've left a proc, methods we do nothing
#       if [catch {
#                   .lbp_console.cframe.text configure -bg $::___zz___(white) -fg $::___zz___(black)
#       } err_code] {
#       }
        incr ::___zz___(trace-level)
        set prc [lindex  $args 0 0] ;# get the proc name from the trace input
        set zzz [namespace exist _$prc] ;# first time a proc is called there's no namespace to clear up
        after 100 [list catch "wm title .lbp_console $prc"]
        if { $zzz } {
#           wait 1000
            if [catch {
                namespace delete _$prc
            } err_code] {
                puts $err_code 
            }
#           wait 1000
        }
    
# ------------------------------------------------------  namespace lookup  --------------------------------------------

    } elseif { $func eq "names" } { #
        set  ns  [lindex $args 0]
        set  var [lindex $args 1]
        puts "lookup in namespace $ns, the var $var"
        return [namespace eval $ns [list namespace which -variable $var]]
    
# ------------------------------------------------------  set debug delay to slowly watch window rebuild  --------------

    } elseif { $func eq "delay" } { #set the delay factor for debugging
        if { [lindex $args 0] eq "" } {
            puts stderr "delay is now $::___zz___(delay)"
        } else {
            puts stderr "set delay to [lindex $args 0]"
            set ::___zz___(delay) [lindex $args 0]
        }
    
# ------------------------------------------------------  skip modulo for reporting skip progress when bp messages enabled  --------------------------------------

    } elseif { $func eq "smod" } { #set the skip modulo
        puts stderr "set skip modulo to [lindex $args 0]"
        set ::___zz___(skip_modulo) [lindex $args 0]
    
# ------------------------------------------------------  kill all the windows in the (vws) list  -----------------------

    } elseif { $func eq "clean" } { #close all vw+ windows, from the ___zz___(vws) list
        set keeplist [list]
        foreach window $::___zz___(vws) {
            if { [lindex $args 0] eq "code" } {
                if { [info command ${window}.sw.f.frame.l0] ne "" } {
                    set value [ ${window}.sw.f.frame.l0 cget -text]
                }
                if { [info command ${window}.l0] ne "" } {
                    set value [ ${window}.l0 cget -text]
                }
                set cd "::[string range $window 1 end]::___[string range $window 2 end]"
                if { $value eq  $cd} {
                } else {
                    lappend keeplist $window
                    continue ;# next, so bypass the close
                }
            }
            puts "close window= |$window| "
            destroy $window
        }
        
        set ::___zz___(vws) $keeplist                   ;# keep all data if a code operation, otherwise keep none
        set ::___zz___(gangcb) 0
        return
# ------------------------------------------------------  manual all the windows in the (vws) list  -----------------------

    } elseif { $func eq "manual" } { #manual all vw+ windows, from the ___zz___(vws) list
        foreach window $::___zz___(vws) {
            if { [info command ${window}.sw.f.frame.f2.cb7] ne "" } {
                set var [ ${window}.sw.f.frame.f2.cb7 cget -variable]
                set $var 1
            }
        }
        return
# ------------------------------------------------------  kill something  -----------------------------------------------
    } elseif { $func eq "kill" } { 
    
# ------------------------------------------------------  debug command, open window with debug data  ------------------------

    } elseif { $func eq "debug" } { ;# a vw+ window with many debugger array values
        if { [info exist ::t_name] } {
            set debugger_name "debugger_$::t_name"
        } else {
            set debugger_name "debugger_main"
        }
        $::___zz___(vw+) {  ::___zz___(bpnum)           ::___zz___(cache)       ::___zz___(max_size)    ::___zz___(proc_wid)    ::___zz___(delay)       
                            ::___zz___(lbp-lock)        ::___zz___(lbp+,pproc)  ::___zz___(lbp+,pline)  ::___zz___(lbp+,line)   
                            ::___zz___(cb1)             ::___zz___(delaya)      ::___zz___(skips)       ::___zz___(delayb_count)
                            ::___zz___(skip_modulo)     ::___zz___(goto)        ::___zz___(go-window)   ::___zz___(lbp+,line)   ::___zz___(delayb)       
                            ::___zz___(bp_messages_default)                     ::___zz___(updatesome)  ::___zz___(updatesomeN) ::___zz___(gang) ::___zz___(vws) 
                            ::___zz___(fontsize)        ::___zz___(deadman)     ::___zz___(deadman2)    ::___zz___(noflylist)   ::___zz___(forcerefresh)
        }  $debugger_name ;#  ::___zz___() 
# ------------------------------------------------------  lp command, functional back to caller  ------------------------

    } elseif { $func eq "lp" } {
        # was proc  lp {{namepat *}} # list procedure(s)
        set namepat [lindex $args 0]
        if { $namepat eq "" } {
            error "wrong number of args: should be $::___zz___(util+) lp procedure-name"
        }

    
# ------------------------------------------------------  old lp for reference only  ------------------------------------


if { 00 } {
        foreach proc [info procs $namepat] {
            set space ""
            puts -nonewline "#---------------------\nproc $proc {"
                foreach arg [info args $proc] {
                    if [info default $proc $arg value] {
                        puts -nonewline "$space{$arg $value}"
                    } else {
                        puts -nonewline $space$arg
                    }
                    set space " "
                }
                # No newline needed because info body may return a
                # value that starts with a newline
                puts -nonewline "} {"
                puts -nonewline  [info body $proc]
            puts "}"
        }
#here's heinrich martin's version: better, but I don't grok it
        
        xproc lp3 {{namepat *}} {
            set ans [lmap p [uplevel 1 info procs [list $namepat]] {
                set globp [uplevel 1 namespace which -command [list $p]]
                set args [lmap arg [info args $globp] {
                    if {[info default $globp $arg val]} {
                        list $arg $val
                    } else {
                        list $arg
                    }
                }]
                list proc $p $args [info body $globp]
            }]
            return  $ans
        }
        
        
        
        
}
        
            # let's use the functional version of this  , leaving my orginal above  
            set proc [lindex $args 0]
        
            set result ""
            set space ""
            set result "proc $proc \{"
            foreach arg [info args $proc] {
                if [info default $proc $arg value] {
                    append result "$space\{[list $arg $value]\}"
                } else {
                    append result $space[list $arg]
                }
                set space " "
            }
            append result "\} \{"
            append result [info body $proc]
            append result "\}\n"
            return $result
        
    } elseif { $func eq "tearoff" } {   ;# line up all the windows
        set geom [wm geom .lbp_console]
        set re {^([0-9]+)x([0-9]+)([+-])([+-]?[0-9]+)([+-])([+-]?[0-9]+)}
        regexp $re $geom -> dx dy xs xpos ys ypos
        set geomt [wm geom [lindex $args 1 ]]
        regexp $re $geomt -> dx dy xs xpost ys ypost
        set newx $xpos
        set newy [expr {   $ypos + 40   }]
        wm geom [lindex $args 1 ] ${dx}x$dy+${newx}+${newy}

# ------------------------------------------------------  command usage help  -------------------------------------------


    } elseif { $func eq "?" } {
        puts "util+ help: "
        puts "     lp <procedure>       display the current code for a proc "
        puts "     tabsize N            set tabsize in code window "
        puts "     smod    #            set the modula for reporting on skiping (now $::___zz___(skip_modulo))"
        puts "     clean                close all the data windows #= [llength $::___zz___(vws)]"
        puts "     grid ?y-extra? ?x-incr? reposition all data windows uses 15 / 500 for y/x"
    } elseif { $func eq "stuff" } {
        dothis-stuff
    } else {
        error "invalid util+ function, should be one of lp, fontsize, smod, clean, tabsize ... or ?"
    }
}

    
# ----------------------------------- lbp + ------------  command, not as a variable  -----------------------------------


#$::___zz___(lbp+)
proc lbp+ { {comment {}} {bpid {}} {tailed 0}} { ;# breakpoint from within a proc, will create a window with local vars, id optional
    if { [incr ::___zz___(deadman2) -1]  < 0} {
        update ;# to avoid freezing totally
        set  ::___zz___(deadman2) $::___zz___(deadman)
    }

    
# ------------------------------------------------------  lbp + command, see if can we get out quickly  -----------------
#set ::___zz___(delayb) 1        ;# spinbox for changing precision, how many instructions per bp's animation
#set ::___zz___(delayb_count) 1  ;# remaining instructions per bp's animation, but only if g values set, i.e. single step always just one

# summary of controls, 
# cb1 - the checkbox for no breakpoints at all
# delaya - the time to delay at the lower level breakpoing (bp+)
# skips - when a g is given with a +N it means skip this many breakpoints
#         when it has a negative value, it means go till that line num 
# skip_modulo - used to only report (if reporting on) every so often, so not to flood
# goto  - the line number we're going to, we set this to 999999 to do a "run" 
# delayb, delayb_count work together to delay doing a breakpoint, i.e. the precision guy
#
    incr ::___zz___(delayb_count) -1 ;# make sure this get's done, no biggy if we ever did this too often or missed one though
    if { $::___zz___(level) > 0} {
        if { [incr  ::___zz___(level_message_count) ] > 10  } {
            return
        }
        puts stderr "no recursive breakpoints allowed, ignoring, level = $::___zz___(level) / $::___zz___(level_message_count) "
        return
    }
    if { $::___zz___(cb1) && $::___zz___(delaya) <= 0} { ;# get out quickly if no breakponts, also don't update values, but if a delay set, continue
        incr ::___zz___(bpnum)
        return
    }
    if {   ($::___zz___(skips)  <= 0)   ||  (  ( $::___zz___(skips)% $::___zz___(skip_modulo) ) == 0    )       } {
    } else {
        incr ::___zz___(skips) -1
        incr ::___zz___(bpnum)
        return
    }
    if { $::___zz___(delayb_count) > 0} {;# should we get outa here, but if we are in run mode (goto >0 ) or skips <= 0 we are stepping 
        if { $::___zz___(goto) < 0 && $::___zz___(skips) <= 0} { #; we are stepping, so don't quit
            set ::___zz___(delayb_count) 0
        } else {
            incr ::___zz___(bpnum)
            return
        }
    } else {
        set ::___zz___(delayb_count) $::___zz___(delayb) ;# don't need this going negative forever
    }
    set level [info frame] ;# we need to go up a level or so to get the variables, we copy them to a namespace
    incr level -1
    set frm_dict  [info frame  $level ] 
    set proc_name ""
    set zzz [dict exists $frm_dict proc]
    set amethod 0
    if { $zzz == 0 } {
        set zzz [dict exists $frm_dict method]
        if { $zzz } {
            set class [dict get $frm_dict class]
            set method [dict get $frm_dict method]
            set cmd [dict get $frm_dict cmd]
            if       { $method eq "<constructor>" } {
                set code [info class constructor $class]
            } elseif { $method eq "<destructor>" } {
                set code [info class destructor $class]
            } else {
                if [catch {
                    set code [info class definition $class $method]
                } err_code] {
                    puts $err_code 
                    vwait forever
                }
            }
            
            
            set args [lindex $code 0]
            set body [lindex $code 1]
            set vars [uplevel 1 {info vars}]
            set myself [uplevel 1 {self}]
            set call [info class call $class $method]
            set the_method "method $method \{$args\} \{ $body \}"
            if { 0 } {
                foreach var $vars {
                    if [catch {
                        set val [uplevel 1 set $var]
                        puts "   var= |$var| val= |$val| "  
                    } err_code] {
                        puts "---error for var= $var -> $err_code "
                    }
                }
            }
            set amethod 1
#           return
        } else {
            puts "its neither a method or a proc"
            return  
        }
    }
        
# ------------------------------------------------------  get proc_name and ns if a proc, if a method, we get the same info above ----------------------------------
    if { $amethod } {
#       return
        set proc_name $method
        set ns _$proc_name
    } else {
        set proc_name [lindex [dict get $frm_dict proc] 0]
        set ns [string map {{::} {_}} $proc_name]
    }       
# ------------------------------------------------------  setup to display instrumentation but one last escape for no local breakpoints here, but incr the count ---
    set show_instr $::___zz___(showinstr)
    if { [info exist ::___zz___(cb3,.$ns)]  && $::___zz___(cb3,.$ns) && $::___zz___(delaya) <= 0} {
        incr ::___zz___(bpnum)
        return
    }
        
# ------------------------------------------- * --------  get the list of user variables  -------------------------------
    set vars [uplevel 1 {info vars}]
# ------------------------------------------------------  get the list of user variables end ----------------------------
        
        
        
# need some functions here, but don't want to polute the command name space any more
        
# ------------------------------------------------------- get_proc_code $proc_name --------------------------------------
    if { $amethod } {
        set proc_def $the_method
        set proc_name $method
#       return
    } else {
        set proc_def [apply {proc {
        
                set result ""
                set space ""
                set result "proc $proc \{"
                foreach arg [info args $proc] {
                    if [info default $proc $arg value] {
                        append result "$space\{[list $arg $value]\}"
                    } else {
                        append result $space[list $arg]
                    }
                    set space " "
                }
                append result "\} \{"
                append result [info body $proc]
                append result "\}\n"
                return $result
                
        }} $proc_name]
    }
    
# ------------------------------------------------------- insert line numbers and show or hide instrumentation commands ----------------------
#   numberit {pdef string} 
    set search_id $comment
    if { $bpid ne "" } {
        set search_id $bpid
    }
    set ::___zz___(lbp+,pline) $::___zz___(lbp+,line) 
        
    set proc_def [apply {{pdef string message show_instr} {
            
            set lines [split $pdef "\n"]
            set num 0
            set out ""
            set next_one 0
            foreach line $lines {
                set cur "  "
                if { $next_one && [string trim $line] ne ""} {
#                   set cur "->"
#                   set cur "\u27F6"
#                   set cur " \u279C" ;# nice, shifts line number over each time though
#                   set cur "\u27FC" ;# a line like |--> little bit of a wobble
                    set cur $::___zz___(arrow) ;# let user decide in config
                    set next_one 0
                }
# ------------------------------------------------------- Save Line Number so bp+ can quit early if we're at the line ------------------------
                if { $string ne "" && [string match  *${string}* $line] } {
                    set cur "--"
                    set ::___zz___(lbp+,line)  [expr {(    $num + 1   )}] 
                    if { $message eq "step-instrument" } {
                        set next_one 1
                        set cur "  "
                    }
                }
                if { ! $show_instr } {
                    set zzz [regsub -nocase -linestop -lineanchor -all {^.*;# instrument-show-begin(.*);# instrument-show-end$} $line {\1 } line]
                    if { $zzz <= 0 || 1} { ;# let's do this all the time, shouldn't hurt, but I'll leave in the if test as a reminder
                        # didn't match, so line comes out the same, so now just test for our instrumentation to hide, if did match, we extract original comment only
                        set zzz 0
                        set zzz [regsub  {\;lbp\+ step\-instrument.*$} $line "" line]
                    }
                    if { $num < 1} {
                        regsub -nocase -linestop -lineanchor -all {(^method.*\{)([ \t]*catch.*init_method.*info vars\]\})} $line {\1} line
                    }
                    
                }
                append out "${cur}[format %4d [incr num]]\t$line\n" 
            }
            return $out
            
            
    }} $proc_def $search_id $comment $show_instr]
    
#   if { $amethod } {
#       return
#   }
# ------------------------------------------------------  get the list in the namespace to  compare to vars list      ------------------------

    set winex 0
    set varsdiff 0 ;# variables are different if 1, assume they are the same
    if { [info command .${ns} ] ne ""} { ;# construct list from namespace, remove the proc we add which is not a true var
        set winex 1
        set allx [info var ${ns}::*]
        set allx2 "::${ns}::__${ns}"
        set catx {}
        foreach item $allx {
            if { $item ne $allx2 } {
                lappend catx [namespace tail $item]
                if [catch {
                    set ::___zz___(temp,0) [set $item]
                } err_code] {
                    set varsdiff 1  
                    break
                }
                set lvar [namespace tail $item]
# ---------------------------------------- + -----------  check if the variable already exists but just changed value   ------------------------
                if [catch {
                    set cmd "expr {\$::___zz___(temp,0)  == \$\{${lvar}\} } " ;# will get an error if the variable is an array - could optimise here, but not yet, we just let it do full update to get the indices correct   
                    set zzz [uplevel 1 $cmd  ]
                    if { $zzz == 0} {
                        set cmd "set $lvar"
                        set zzz1 [uplevel 1 $cmd  ] ;# get the new value for the variable, try to set the var in the namespace so it updates
                        set $item $zzz1 ;# just update item to new value
                        set zzz 1       ;# and reset this, later, we might find we have a new variable, and the var lists won't match, so we then do a full update, but not yet needed
                    }
                } err_code err_code2] {
                    set zzz 0 ;#if we get an error, then just consider it to be different, so we call the full update 
                }
#               set cmd "set $lvar"
#               set zzz1 [uplevel 1 $cmd  ]
                if { $zzz == 0} {
                    set varsdiff 1  
                    break ;# don't need to check further, they are different, we need to do a full update
                }
            }   
        }
        set catx [lsort $catx] ;# the final result, but only if winex is 1
    } else {
    }
    
# ------------------------------------------------------  put in first variable the proc, into the namespace for this proc -------------------
    
    
    
    set pdef "variable __$ns \{\n$proc_def \n \}\n" ;# not a problem here with quoting, since it's a valid proc we got back, so quoting should be correct
    
    set ncmd ""
    set ncmd $pdef
    set nvar -1
# ------------------------------------------------------  put into the namespace for this proc each variable from $vars  the locals ----------
    if { $varsdiff || 1} {
        foreach var $vars {
            incr nvar
            set cmd "array exist $var" ;# command to run in caller stack frame
            set arr [uplevel 1 $cmd]   ;# and now run it there
            if [catch {
                set cmd "if \{ ! \[ array exist $var \] \} \{ set $var \} else  \{ lsort \[ array names $var\]  \}     "
                set aval [uplevel 1 $cmd]
                set ok 1
            } err_code] {
                set ok 0
            }
            if { ! $ok } {
                continue
            }
            
#           set f1 "\{"
#           set f2 "\}"
#           set aval [string map  [list $f1 \\$f1 $f2 \\$f2 ] $aval]
            
            
            set ::___zz___(temp,$nvar) $aval
            if { $arr } {
                append  ncmd "variable $var ()\\ \$::___zz___(temp,$nvar) \n"
            } else {
                append  ncmd "variable $var \$::___zz___(temp,$nvar) \n"
            }
        }
        
    }
# ------------------------------------------------------  now use $cmd in the namespace $ns to create/assign values to namespace   -----------
    
    if [catch {
            namespace eval $ns $ncmd
    } err_code] {
    }
    
# ------------------------------------------------------  compare the 2 lists, from namespace and info vars, result is $equal      -----------
    set equal 0 ;# will be 0 if the window doesn't exist, or the variables are not the same
    if { $winex } {
        set catx [lsort $catx]
        set varx [lsort $vars]
        if { [llength $catx] == [llength $varx] } {
            if { [string equal $catx $varx] } {
                set equal 1
            }
        }
    } else {
    }
#   wait 1000
#   vwait forever

# ------------------------------------------- * --------------------- call to get the window updated, by CALLING VW + from here --------------
    if { (! $equal) ||  ($::___zz___(bpnum) % $::___zz___(forcerefresh) == 0 ) } {
#       vwait forever
                                $::___zz___(vw+) "${ns}::" .$ns 
    } else {
        if { $varsdiff } {
                                    update
        } else {
#           update  
            incr ::___zz___(updatesomeN) -1
            if { $::___zz___(updatesomeN) <= 0} { ;# v {::___zz___(updatesomeN) ::___zz___(updatesome)} update_globals
                update 
                set ::___zz___(updatesomeN) $::___zz___(updatesome) 
            }
        }
    }
# ------------------------------------------------------------------- call to get the window updated, by CALLING VW + from here end-----------

#   vwait ::forever

    
    if { [info exist ::___zz___(cb2,.$ns)] && $::___zz___(cb2,.$ns) && [info exist ::___zz___(cb3,.$ns)] && !$::___zz___(cb3,.$ns) } {

#   show_simple $proc_def   $::___zz___(lbp+,line)
    
# ------------------------------------------- * --------  BUILD the Code Window if is does not exist  ----------------------------------------

    if { [info command .lbp_console] eq ""} {
        set ::___zz___(lbp-lock) 0
        set font {Consolas 12}
#       set font {TkFixedFont 12}

        toplevel .lbp_console
        frame .lbp_console.bframe ;# frame with buttons
        frame .lbp_console.cframe ;# frame with program text


        
        text  .lbp_console.cframe.text -height 25 -wrap none -font $font -tabs "[expr {$::___zz___(tabsize)* [font measure $font 0]}] left" -tabstyle wordprocessor -width 24 -yscrollcommand [list .lbp_console.cframe.y set] -fg $::___zz___(black) -bg  $::___zz___(white)
        scrollbar .lbp_console.cframe.y -orient vertical -command [list  .lbp_console.cframe.text yview]
        
        button .lbp_console.bframe.b0    -text "EXIT"    -command {exit} ;# -image $image ;#
#       button .lbp_console.bframe.b1    -text "Clear"   -command {.lbp_console.cframe.text delete 1.0 end} ;# -image $image ;#


        label  .lbp_console.bframe.b1    -text "Menu" -relief raised
        
        set box1 "  "
        set boxc "\u25A1 "
        set boxr "\u25A3 "
        # Create a menu
        set m [menu .lbp_console.menu1 -tearoff 1 -tearoffcommand [list $::___zz___(util+) tearoff]]
        menu $m.extra       -tearoff 0 
        $m add command      -label     "${box1}Arrange Data Windows"            -command "[list $::___zz___(util+) grid];raise .lbp_console" -font TkFixedFont
        $m add checkbutton  -label     "${boxc}Move as a Group      "           -variable ::___zz___(gangcb)                    -indicatoron 1  -font TkFixedFont   -command "[list $::___zz___(util+) gridck];raise .lbp_console"
        $m add separator                    
        $m add command      -label     "${box1}Close Data Windows"              -command [list $::___zz___(util+) clean]        -font TkFixedFont
        $m add command      -label     "${box1}Close Code Windows"              -command [list $::___zz___(util+) clean code]   -font TkFixedFont
        $m add command      -label     "${box1}Manual Geom - Set All"           -command [list $::___zz___(util+) manual]       -font TkFixedFont
        $m add separator 
        $m add command      -label     "${box1}List Instrumented  "                           -command {vwdebug::listinstr}                       -font TkFixedFont
        $m add command      -label     "${box1}Choose Procs to Instrument "     -command {vwdebug::chooseprocs}                 -font TkFixedFont
        $m add checkbutton  -label     "${boxc}Show instrument+ Code"           -variable ::___zz___(showinstr)                 -indicatoron 1  -font TkFixedFont   -command "[list $::___zz___(util+) showlines %W %s %d] ; set ::___zz___(coverage) 0"
        $m add separator                    
        $m add command      -label     "${box1}Clear Code Window"               -command {.lbp_console.cframe.text delete 1.0 end}              -font TkFixedFont
        $m add command      -label     "${box1}Bottom of Code Window"           -command {.lbp_console.cframe.text see end; .lbp_console.cframe.text mark set insert end}   -font TkFixedFont
        $m add checkbutton  -label     "${boxc}Minimal Update"                  -variable  ::___zz___(minupdate)                -indicatoron 1  -font TkFixedFont   -command {if    {$::___zz___(minupdate) == 0    } {  set ::___zz___(coverage)  0     }}
        $m add checkbutton  -label     "${boxc}Coverage Tracks"                 -command {if    {$::___zz___(coverage) == 0 } { .lbp_console.cframe.text delete 1.0 end    } else {set ::___zz___(minupdate) 1}}   -variable  ::___zz___(coverage) -indicatoron 1  -font TkFixedFont   
        $m add separator                    
        $m add command      -label     "${box1}List All Frames"                 -command [list $::___zz___(util+) frames-callback]              -font TkFixedFont
        $m add command      -label     "${box1}List Cmd Frames"                 -command [list $::___zz___(util+) frames-callback2]             -font TkFixedFont
        $m add separator                    
        $m add command      -label     "${box1}Widget Browser"                  -command {vwdebug::wtree}                                       -font TkFixedFont
        $m add separator                    
        $m add cascade      -label      "${box1}Extra" -menu $m.extra           -font TkFixedFont
            $m.extra add command      -label     "${box1}No bp Msgs - check all"            -command [list $::___zz___(util+) no-bp-messages-all]       -font TkFixedFont
            $m.extra add checkbutton  -label     "${boxc}No bp Msgs - default value"        -variable ::___zz___(bp_messages_default)   -indicatoron 1  -font TkFixedFont   
            $m.extra add separator                   
            $m.extra add radiobutton  -label     "${boxr}Small  Data  Font (close 1st)"     -variable ::___zz___(fontsize)  -value 8        -font TkFixedFont
            $m.extra add radiobutton  -label     "${boxr}Medium Data  Font"                 -variable ::___zz___(fontsize)  -value 10       -font TkFixedFont
            $m.extra add radiobutton  -label     "${boxr}Larger Data  Font"                 -variable ::___zz___(fontsize)  -value 12       -font TkFixedFont
        
        bind .lbp_console.bframe.b1 <1> {tk_popup .lbp_console.menu1 %X %Y}
    
        
        
#       button .lbp_console.bframe.b2    -text "Bottom"  -command {.lbp_console.cframe.text see end; .lbp_console.cframe.text mark set insert end} ;# -image $image ;#
        checkbutton .lbp_console.bframe.b2a    -text "lock" -variable ::___zz___(lbp-lock) -relief raised ;# -image $image ;# -command $tcmd 

        button .lbp_console.bframe.b3    -text "Font --" -command [list $::___zz___(util+) fontsize .lbp_console.cframe.text -1] ;# -image $image ;#
        button .lbp_console.bframe.b4    -text "Font ++" -command [list $::___zz___(util+) fontsize .lbp_console.cframe.text 1] ;# -image $image ;#
        button .lbp_console.bframe.b5    -text "Console" -command {catch {console show}} ;# -image $image ;#
        button .lbp_console.bframe.b6    -text "Stop"    -command {set ::___zz___(skips) 1;set ___zz___(goto) -1} ;# -image $image ;#
        button .lbp_console.bframe.b7    -text "Go/Step"     -command [list $::___zz___(go+)]  ;# -image $image ;#
        button .lbp_console.bframe.b9    -text "Run"     -command [list $::___zz___(go+) -999999]  ;# -image $image ;#
        
        set tcmd "wm attributes .lbp_console -topmost \$::___zz___(lbp-ontop)"
        checkbutton .lbp_console.bframe.b8    -text "On Top" -variable ::___zz___(lbp-ontop) -command $tcmd  -relief raised ;# -image $image ;#
        
        set ::___zz___(entry1) ""
        set ::___zz___(entry3) ""

        frame .lbp_console.xframe ;# frame with command execute entry (I give up trying to get the buttons/entry to line up with the default font, so use a fixed size one)
        button .lbp_console.xframe.lab3a -text "Command:" -font {courier 10} -command {set ::___zz___(entry1) "";focus .lbp_console.xframe.entry } ;#-font {courier 14}
        entry .lbp_console.xframe.entry -text "entry" -textvariable ::___zz___(entry1) -font {courier 14} ; #set ::___zz___(entry1) "set args"
        bind  .lbp_console.xframe.entry <Key-Return> [list $::___zz___(util+) enter-callback 2 %W %K]
        bind  .lbp_console.xframe.entry <Key-KP_Enter> [list $::___zz___(util+) enter-callback 2 %W %K]
        bind  .lbp_console.xframe.entry <Key-Up> [list $::___zz___(util+) enter-callback 2 %W %K]
        bind  .lbp_console.xframe.entry <Key-Down> [list $::___zz___(util+) enter-callback 2 %W %K]
        bind .lbp_console.xframe.entry <MouseWheel> [list $::___zz___(util+) enter-callback 2 %W %D]
        bind .lbp_console.xframe.entry <4>          [list $::___zz___(util+) enter-callback 2 %W 1]
        bind .lbp_console.xframe.entry <5>          [list $::___zz___(util+) enter-callback 2 %W -1]
        
        frame       .lbp_console.uframe ;# frame with uplevel command execute entry
        button      .lbp_console.uframe.lab3c   -text "Uplevel:"    -font {courier 10} -command {set ::___zz___(entry3) ""; focus .lbp_console.uframe.entry} ;#-font {courier 14} 
        entry       .lbp_console.uframe.entry   -text "entry"       -textvariable ::___zz___(entry3) -font {courier 14}

        label       .lbp_console.uframe.label   -text "Delay" -relief raised -bd 0

 #      ------------------------------------------------- mouse wheel windows and linux

        spinbox     .lbp_console.uframe.sbox    -from 0             -to 999     -increment 1  -textvariable ::___zz___(delaya) -width 3 -font {courier 14}
        bind        .lbp_console.uframe.sbox  <MouseWheel> {apply [list {spinner value} { 
                                                                if { $value > 0 } {
                                                                    $spinner invoke buttonup
                                                                } else {
                                                                    $spinner invoke buttondown
                                                                }
                                                            } ] %W %D}                                                          
        bind        .lbp_console.uframe.sbox  <Button-4> {apply [list {spinner} { 
                                                                    $spinner invoke buttonup
                                                            } ] %W}                                                         
        bind        .lbp_console.uframe.sbox  <Button-5> {apply [list {spinner } { 
                                                                    $spinner invoke buttondown
                                                            } ] %W} 
                                                                                                                    
 #      -------------------------------------------------



        spinbox     .lbp_console.uframe.sbox100     -from 0             -to 999     -increment 100  -textvariable ::___zz___(delaya) -width 3 -font {courier 14}
        bind        .lbp_console.uframe.sbox100  <MouseWheel> {apply [list {spinner value} { 
                                                                if { $value > 0 } {
                                                                    $spinner invoke buttonup
                                                                } else {
                                                                    $spinner invoke buttondown
                                                                }
                                                            } ] %W %D}                                                          
        bind        .lbp_console.uframe.sbox100  <Button-4> {apply [list {spinner} { 
                                                                    $spinner invoke buttonup
                                                            } ] %W}                                                         
        bind        .lbp_console.uframe.sbox100  <Button-5> {apply [list {spinner } { 
                                                                    $spinner invoke buttondown
                                                            } ] %W} 
                                                                                                                    
 #      -------------------------------------------------
 
        spinbox     .lbp_console.uframe.sbox10  -from 0             -to 999     -increment 10  -textvariable ::___zz___(delaya) -width 3 -font {courier 14}
        bind        .lbp_console.uframe.sbox10  <MouseWheel> {apply [list {spinner value} { 
                                                                if { $value > 0 } {
                                                                    $spinner invoke buttonup
                                                                } else {
                                                                    $spinner invoke buttondown
                                                                }
                                                            } ] %W %D}                                                          

        bind        .lbp_console.uframe.sbox10  <Button-4> {apply [list {spinner} { 
                                                                    $spinner invoke buttonup
                                                            } ] %W}                                                         
        bind        .lbp_console.uframe.sbox10  <Button-5> {apply [list {spinner } { 
                                                                    $spinner invoke buttondown
                                                            } ] %W} 
                                                                                                                    
 #      -------------------------------------------------
            
        label       .lbp_console.xframe.label   -text "Precision" -relief raised -bd 0
                                                            
                                                                                                                    
 #      -------------------------------------------------
            
        spinbox     .lbp_console.xframe.sbox10  -from 0             -to 999     -increment 10  -textvariable ::___zz___(delayb) -width 3 -font {courier 14}
        bind        .lbp_console.xframe.sbox10  <MouseWheel> {apply [list {spinner value} { 
                                                                if { $value > 0 } {
                                                                    $spinner invoke buttonup
                                                                } else {
                                                                    $spinner invoke buttondown
                                                                }
                                                            } ] %W %D}  
                                                                                                                    
        bind        .lbp_console.xframe.sbox10  <Button-4> {apply [list {spinner} { 
                                                                    $spinner invoke buttonup
                                                            } ] %W}                                                         
        bind        .lbp_console.xframe.sbox10  <Button-5> {apply [list {spinner } { 
                                                                    $spinner invoke buttondown
                                                            } ] %W} 
                                                                                                                    
 #      -------------------------------------------------
            
        spinbox     .lbp_console.xframe.sbox    -from 1             -to 999     -increment 1  -textvariable ::___zz___(delayb) -width 3 -font {courier 14}
        bind        .lbp_console.xframe.sbox  <MouseWheel> {apply [list {spinner value} { 
                                                                if { $value > 0 } {
                                                                    $spinner invoke buttonup
                                                                } else {
                                                                    $spinner invoke buttondown
                                                                }
                                                            } ] %W %D}
                                                                                                                        
        bind        .lbp_console.xframe.sbox  <Button-4> {apply [list {spinner} { 
                                                                    $spinner invoke buttonup
                                                            } ] %W}                                                         
        bind        .lbp_console.xframe.sbox  <Button-5> {apply [list {spinner } { 
                                                                    $spinner invoke buttondown
                                                            } ] %W} 
                                                                                                                    
 #      -------------------------------------------------
            
        label       .lbp_console.xframe.labell  -text "Show Lines" -relief raised -bd 0
#       set ::___zz___(proc_wid) 12
        
        
        spinbox     .lbp_console.xframe.sboxl   -from 1             -to 100     -increment 1  -textvariable ::___zz___(proc_wid) -width 3 -font {courier 14} -command [list $::___zz___(util+) showlines %W %s %d]
        bind        .lbp_console.xframe.sboxl  <MouseWheel> {apply [list {spinner value} { 
                                                                if { $value > 0 } {
                                                                    $spinner invoke buttonup
                                                                } else {
                                                                    $spinner invoke buttondown
                                                                }
                                                            } ] %W %D}  
                                                            
        bind        .lbp_console.xframe.sboxl  <Button-4> {apply [list {spinner} { 
                                                                    $spinner invoke buttonup
                                                            } ] %W}                                                         
        bind        .lbp_console.xframe.sboxl  <Button-5> {apply [list {spinner } { 
                                                                    $spinner invoke buttondown
                                                            } ] %W} 
                                                                                                                    
 #      -------------------------------------------------
                                                            
                                                                                                                    
        bind  .lbp_console.uframe.entry <Key-Return> [list $::___zz___(util+) enter-callback 1 %W %K]
        bind  .lbp_console.uframe.entry <Key-KP_Enter> [list $::___zz___(util+) enter-callback 1 %W %K]
        bind  .lbp_console.uframe.entry <Key-Up> [list $::___zz___(util+) enter-callback 1 %W %K]
        bind  .lbp_console.uframe.entry <Key-Down> [list $::___zz___(util+) enter-callback 1 %W %K]
        bind .lbp_console.uframe.entry <MouseWheel> [list $::___zz___(util+) enter-callback 1 %W %D]
        bind .lbp_console.uframe.entry <4>          [list $::___zz___(util+) enter-callback 1 %W 1]
        bind .lbp_console.uframe.entry <5>          [list $::___zz___(util+) enter-callback 1 %W -1]
        
        
        
        pack .lbp_console.bframe    -side top   -expand 0 -fill x
        pack .lbp_console.uframe    -side top   -expand 0 -fill x
        pack .lbp_console.xframe    -side top   -expand 0 -fill x
        
        pack   .lbp_console.xframe.lab3a        -side left -expand 0 -fill none
        pack   .lbp_console.xframe.entry        -side left -expand 1 -fill x 
        pack   .lbp_console.xframe.labell       -side left -expand 0 -fill none 
        pack   .lbp_console.xframe.sboxl        -side left -expand 0 -fill none 
        pack   .lbp_console.xframe.label        -side left -expand 0 -fill none 
        pack   .lbp_console.xframe.sbox10       -side left -expand 0 -fill none 
        pack   .lbp_console.xframe.sbox         -side left -expand 0 -fill none 
        
        pack   .lbp_console.uframe.lab3c        -side left -expand 0 -fill none
        pack   .lbp_console.uframe.entry        -side left -expand 1 -fill x 
        pack   .lbp_console.uframe.label        -side left -expand 0 -fill none 
        pack   .lbp_console.uframe.sbox100      -side left -expand 0 -fill none 
        pack   .lbp_console.uframe.sbox10       -side left -expand 0 -fill none 
        pack   .lbp_console.uframe.sbox         -side left -expand 0 -fill none 
        
        pack .lbp_console.cframe    -side top   -expand 1 -fill both
        
        pack .lbp_console.cframe.text    -side left -expand 1 -fill both
        bind .lbp_console.cframe.text  <Double-Button-1> [list after idle [list $::___zz___(util+) double-click %W %x %y]]
#       bind .t   <Double-Button-1> [list after idle [list $foo double-click %W %x %y]]     
        pack .lbp_console.cframe.y   -side right -expand 0 -fill y
        
        pack .lbp_console.bframe.b0 .lbp_console.bframe.b1 .lbp_console.bframe.b3  .lbp_console.bframe.b4 .lbp_console.bframe.b5 .lbp_console.bframe.b6  .lbp_console.bframe.b9   .lbp_console.bframe.b7   .lbp_console.bframe.b2a .lbp_console.bframe.b8  -fill both -expand true -side left
        if { [info exist ::___zz___(console_geom) ]} {
#           after 100 {wm geom .lbp_console {*}$::___zz___(console_geom) ; update}
            puts "setting lbp console geom $::___zz___(console_geom) "
        } else {
            wm geom .lbp_console 1061x804+-1+185
        }
        
        
        if { $::___zz___(tooltips) != 0 } {
            if [catch {
                package require tooltip
                set delay 1000
                if {       $::___zz___(tooltips) != 0} {
                    set delay  $::___zz___(tooltips)
                } elseif {  $::___zz___(tooltips) == 0 } {
                    set delay 0
                }
                if { $delay > 0 } {
                    tooltip::tooltip delay $delay
                    tooltip::tooltip  .lbp_console.xframe.labell "Show N lines below current line"
                    tooltip::tooltip  .lbp_console.xframe.lab3a  "Clear the command entry, where you can\nenter a command. Runs at global level, however\nso be careful"
                    tooltip::tooltip  .lbp_console.uframe.lab3c  "Clear the uplevel entry, where you can\nenter a command that runs in the stopped proc.\nthe result will be output in the console stderr"

                    tooltip::tooltip .lbp_console.bframe.b1     "Menu of utility commands, left click\ncan tearoff menu with ----------"     
#                   tooltip::tooltip .lbp_console.bframe.b2     "Scroll to Bottom of the window"    
                    tooltip::tooltip .lbp_console.bframe.b2a    "Keep the window steady, may result\nin some lines off screen\nbypasses .text see calls and\njust keeps scrollbar at the top"    
                    tooltip::tooltip .lbp_console.bframe.b3     "Smaller font"   
                    tooltip::tooltip .lbp_console.bframe.b4     "Larger font"   
                    tooltip::tooltip .lbp_console.bframe.b5     "Open the Console - only if in main thread\nthere is no console in tasks"   
                    tooltip::tooltip .lbp_console.bframe.b6     "Stop a go+ command, if running N breakpoints \nor running to line number or run mode\nUse to view code with scrollbar"     
                    tooltip::tooltip .lbp_console.bframe.b7     "Go - one step, or to next breakpoint\ndble click a line # to run to line\ncan use stop to break earlier" 
                    tooltip::tooltip .lbp_console.bframe.b9     "Go - until manually stopped - the animated go\ncan be much slower, but visible" 
                    tooltip::tooltip .lbp_console.uframe.label  "Amount to delay in MS between break-points\nused when a g +/- is active or run mode \nto slow program for better animated viewing \ncan adjust with mousewheel or enter a valid integer\nif > 0 updates can occur in data windows since\nit enters event loop - 3 spinboxes: 100s 10s 1s"       
                    tooltip::tooltip .lbp_console.xframe.label  "Precision, number of statements / breakpoint\nwhen in Run mode. A 0/1 are the same\nif > 1 can miss goto line breaks (dbl click)\nhigher=faster running - 2 spinboxes: 10's and 1's"       

                }
            } err_code] {
                puts stderr "Tooltip error: $err_code" 
                set ::___zz___(tooltips) 0
                namespace eval tooltip {
                    proc tooltip {args} {}
                }
            }
        }
        
    
# ------------------------------------------------------  it exists, so just delete all the text in the window  ------------------------------

    } else {
        if { $::___zz___(skips) <= 0 } {
#           .lbp_console.cframe.text delete 1.0 end
        }
    }

# ------------------------------------------------------ output all lines  new way can scroll all code too     ---------------------------------

# we have $proc_def $::___zz___(lbp+,line)


    set no_result [apply {{code name line} {
    set line1 [expr {   max(1,$line - 1)   }]
    set line2 [expr {   $line + $::___zz___(proc_wid)   }] ;# adjust via spinbox
#   if { $line < 2 } {
#       xputs "name= |$name| line= |$line| size is [string length $code]"
#   }
    if { $::___zz___(lbp+,pproc) ne $name ||  $::___zz___(minupdate) == 0} {
        .lbp_console.cframe.text delete 1.0 end
        .lbp_console.cframe.text insert end-1c "$code\n"
    } else {
        
        set pline [expr {   $::___zz___(lbp+,pline) +1  }]
        set iline [expr {   $line +1  }]  
        if { $pline > 0 } {
            if { [.lbp_console.cframe.text get $pline.0] eq  $::___zz___(arrow)} {
                .lbp_console.cframe.text delete $pline.0 
                if { $::___zz___(coverage) == 0 } {
                .lbp_console.cframe.text insert $pline.0 "  " 
                } else {
                    .lbp_console.cframe.text insert $pline.0 "$::___zz___(carrow) " ;# "||" 
                }
            } else {
                .lbp_console.cframe.text delete 1.0 end
                .lbp_console.cframe.text insert end-1c "$code\n"
            }
        } else {
        }
        if { [.lbp_console.cframe.text get $iline.0] eq  $::___zz___(arrow)} {
        } else {
            .lbp_console.cframe.text delete $iline.0 $iline.2 
            .lbp_console.cframe.text insert $iline.0    $::___zz___(arrow) 
        
        }
        
        

    }
    if { !$::___zz___(lbp-lock)  } {
        .lbp_console.cframe.text see ${line2}.0 
        .lbp_console.cframe.text see ${line1}.0
    }
    set ::___zz___(lbp+,pproc) $name
    }} $proc_def $proc_name $::___zz___(lbp+,line)]
    
#.lbp_console.cframe.text delete 5.0 5.1
#.lbp_console.cframe.text insert 5.0 --
# ------------------------------------------------------ output a range of lines around the current line old ---------------------------------

    
} else {;# check button checks
    set ::___zz___(lbp+,pline) -1
}

    if { $comment eq "" } {
        set colon ""
    } else {
        set colon ": "  
    }



# -------------------------------------------------------------------- do low level breakpoint  ----------------------------------------------
if { $tailed } {
    $::___zz___(bp+) "$ns $colon$comment" 0 1 1 ;# and finally, we call the regular breakpoint if breakpoints not disabled
} else {
    $::___zz___(bp+) "$ns $colon$comment" $::___zz___(cb3,.$ns) $::___zz___(cb4,.$ns) ;# and finally, we call the regular breakpoint if breakpoints not disabled    
}
    

# -------------------------------------------------------------------- do low level breakpoint end -------------------------------------------

    set ncmd ""

    
# ------------------------------------------------------ handle the variables  going back after we continue from the breakpoint --------------

    foreach var $vars {
        set cmd "array exist $var" ;# command to run in caller stack frame
        set arr [uplevel 1 $cmd]   ;# and now run it there
        if [catch {
            set cmd "if \{ ! \[ array exist $var \] \} \{ set $var \} else  \{ lsort \[ array names $var\]  \}     "
            set aval [uplevel 1 $cmd]
            set ok 1
        } err_code] {
            set ok 0
        }
        if { ! $ok } {
            continue
        }
        set nsvar "::${ns}::${var}" ;# the variable our namespace, so we can push it back to the locals
        set comment_it_out ""
        if { $var eq "args" } {
#           set comment_it_out "#" ;# don't think it's a good idea to have the user change args, maybe we will change our minds
        }
        if { $arr } {
            append  ncmd "#array set $var \{() $nsvar\} ...\n" ;# here is where would could someday support local array variables
        } else {
#           append  ncmd "${comment_it_out}set $var \$$nsvar \n puts \"$var now is \$$var\" \n   " ;# debug in the proc's space
            append  ncmd "   ${comment_it_out}set $var \$\{${nsvar}\}  \n"
        }
    }
    uplevel 1 $ncmd
#   namespace eval $ns $ncmd ;# set the variables in the user's local
#   $::___zz___(vw+) "${ns}::" .$ns

    if {$::___zz___(tail) == 1} {
        set ::___zz___(tail) 0
        tailcall lbp+ $comment $bpid 1
    }

} 
# ------------------------------- instrument + --------- instrument code with lbp + breakpoints ----------------------------------------------

proc instrument+ {procedure args} {
# this is a work in progress. it will add single stepping breakpoints
# to the code but there are cases where it won't work, like when there
# are switch statements on several lines. it appends a ;lbp+ step-instrument id
# where id is generated as a large number so it should be unique
# to use, do this:   eval [instrument+ procname]
# after the proc has been defined, this will re-define it with debug code
    set lbracket "\{"
    set rbracket "\}"
    set no_warn 0
    set nw [lsearch -exact $args -nowarn]
    if { $nw > -1 } {
        set no_warn 1
        set args [lreplace $args $nw $nw]
    }
    if { $procedure eq "-pick" } {
        tailcall vwdebug::chooseprocs {*}$args
    }
    if { $procedure eq "-list" } {
        tailcall vwdebug::listinstr {*}$args
    }
# -------------------------------------------------------- instrument class methods, by faking it, quite a hack, but hey...
    if { $procedure eq "-class" } { ;# instr -class class method
    
        set theclass [lindex $args 0 ]
        if { [llength $args] > 2 } {
            foreach item [lrange $args 1 end] {
                instrument+ -class $theclass $item  
            }
            return ""
        }
        set themethod [lindex $args 1 ]
        if { $themethod eq "*" } {
            foreach item [info class methods $theclass -private] {
                if { $no_warn } {
                    instrument+ -class $theclass $item -nowarn  
                } else {
                    instrument+ -class $theclass $item  
                }
            }
            return ""
        }
        
        if [catch {
            oo::define $theclass export $themethod
        } err_code] {
            puts stderr $err_code 
        }
        if { [info exist ::___zz___(originalm,$themethod)] } {
            error "Method \"$themethod\" is already instrumented (cannot -revert)"
        }
        set def [info class definition $theclass $themethod]
        set arglist [lindex $def 0]
        set mcode [lindex $def 1]
        set temp "proc ${theclass}__z__$themethod \{$arglist\} \{ $mcode\}\n" ;# construct a fake proc
        eval $temp ;# now define the new fake proc, so we can instrument it normally
        set result [instrument+ ${theclass}__z__$themethod -noeval] ;# instrument this fake proc
        set lines0 [split $result \n]
        set lines [lrange $lines0 1 end-3] ;# remove the 2 traces at the end - note, these traces are what clear the namespace on each entry, so we have a fresh one with no locals - caused the method var bug by not having them
        set len [string first "\{" [lindex $lines0 0]]
        set line1 [string range [lindex $lines0 0] $len-1 end]
        set line1 [regsub {;lbp\+} $line1 "catch {$::___zz___(util+) init_method  _$themethod \[info vars\]} ;lbp+"]

        set traces [lrange $lines0 end-2 end] ;# eventually we need to see if we can trace a method, -- no, there is no trace for methods, so we now add a statement at the beginning of a method to use the new init_method, and note comment above
        set newline1 "oo::define ${theclass} $lbracket method $themethod $line1 \n" ;# rebuild the method def
        set result $newline1 ;# start with a new line 1, which is the oo::define and method declaration
        foreach item [lmap xxx $lines {string cat $xxx \n}] {
            append result $item 
        }
        append result "\n$rbracket\n"
        rename ${theclass}__z__$themethod {} ;# get rid of the temporary proc we built so we could instrument it
        append result "oo::define $theclass export $themethod\n"
        eval $result        
        set ::___zz___(originalm,$themethod) $theclass      
        return "" ;# there is no -revert for methods, if the result is eval'd not a problem (since we used to require that)
    }

# -------------------------------------------------------- instrument a proc ...
    set frames [vwdebug::get_frames]
    set frames_trm {}
    foreach item $frames {
        lappend frames_trm [string trimleft $item :]    
    }
    set enable 1 ;# enable instrumenting
#   set pcode [getproc $procedure]
    set norb 0
    set noeval 0
    if { [lsearch "-norb" $args] >= 0 } {  
        set norb 1
    }
    if { [lsearch "-noeval" $args] >= 0 } {  
        set noeval 1
    }
    set procedure  [string trimleft $procedure ":"]
    if { $procedure in $frames_trm } {
        error "cannot change \"$procedure\" when it's in the active call frames: \{$frames_trm\}"
    }
    if { [lsearch "-revert" $args] >= 0 } { 
        if { [info exists  ::___zz___(original,$procedure)] } {
            eval $::___zz___(original,$procedure) ;# this automatically removes the trace, so we need not do it ourselves
            unset ::___zz___(original,$procedure) ;# remove this so it will fail if user tries it twice
#           set zzz [trace info execution $procedure]
#           trace remove execution $procedure enter [list $::___zz___(util+) tracer]
#           set zzz [trace info execution $procedure]
        } else {
            error "cannot find an original to revert to for $procedure - was it already reverted?"
            return
        }
        puts stderr "$procedure reverted to original"
        return ""
    }
    if { [info exists ::___zz___(original,$procedure)] } {
        error "The proc \"$procedure\" is already instrumented, you must -revert first  [info exists ::___zz___(original,$procedure)]"
        return
    }
    set pcode [apply { {proc} {
    
        set result ""
        set space ""
        set result "proc $proc \{"
        foreach arg [info args $proc] {
            if [info default $proc $arg value] {
                if [info default $proc $arg value] {
                    append result "$space\{[list $arg $value]\}"
                } else {
                    append result $space[list $arg]
                }
            } else {
                append result $space$arg
            }
            set space " "
        }
        append result "\} \{"
        append result [info body $proc]
        append result "\}\n"
        return $result
        
                
    }} $procedure] 
# lets stash away pcode, which has the original, so we can get it back, but only if we do the eval
    if { $noeval == 0} {
        set ::___zz___(original,$procedure) $pcode
        
    }


    set lines [split [string trimright $pcode \n] \n]
    set nlines [llength $lines]
    set ln 0
    set idn 1000000000
    set out ""
    
    set scrub 1
    set lrev [lreverse [lrange $lines 0 end-1 ] ]
    
    foreach line $lrev {
        if       { [string trim $line] eq "" } {
            incr scrub
            continue
        } elseif { [string index [string trim $line] 0]  eq "#"  } {
            incr scrub
            continue
        } else {
            break
        }
    }
    set scrub  [expr {   $nlines - $scrub  - 1 }]

# at this point, scrub is the number of lines at the end we won't instrument, since we can't have anything after the final
# statement in the proc, since that's the proc's return value iff there's no explicit return, so we can't instrument that
# line or anything after it. So, we use the enable/disable code and turn off enable for the last set of lines
# note this is not perfect, there could be an if statement with a result, best to always use explicit returns

    set return_seen 0   
    set skipit 0
    foreach line $lines {
        incr ln
        if { $ln > $scrub } {
            set enable 0
        }
        regsub {[ \t];#[ \t].*$} $line {} tline
        if { [regexp {^[ \t]*return[ \t]*.*$} $line ]} {
            set return_seen 1   
        }
        if { $skipit > 0 } { ;# we have a partial command across lines, don't instrument (only for set and puts now)
            incr skipit -1
#           append out "$line ;# from skipit $skipit\n"
            append out "$line\n"
            continue
        }
        set tline [string trim $tline]
        set ok [info complete $tline] ;# not sure exactly how this works, but if its not complete, we may not want to instrument it
        if { $ok } {
            if { [regexp {^.*\{[ \t]*return[ \t]*.*\}$} $line ]} { ;# looking for a one line proc with a return
                set return_seen 1   
            }
        } else { ;# not a complete line, call utility to get the number of lines forward till we complete the command
            if { [regexp {^[ \t]*(set|puts)[ \t]*.*$} $line ] || 1} { ;# looking for a set or puts statement that is incomplete
                set skipitl [$::___zz___(util+) completeit $nlines $ln $lines]
                set skipit [lindex $skipitl 0 ]
                set cmd [lindex $skipitl 1 ]
#               update
                set cmdname [string trimleft $cmd]
                set zzz [regexp {^(\w+)[ \t]*} $cmdname -> cmdname]
                
                incr skipit -2  
                if { $cmdname in {if while for foreach while proc} || $zzz == 0} {
                    set skipit 0
                }
            }
            
        }
        # ok is now either 0 or 1, next we check for special cases and adjust ok based on that, but might just drop through
        set enable_seen 0
        if { [string index $tline 0] eq "#"} { ;# want to skip lines beginning with # so we don't step through comments
            
            if       { [string range $tline 0 7] eq  "#enable+" } {
                set enable 1
                set tline "" 
#               set line ""
                set enable_seen 1
            } elseif { [string range $tline 0 8] eq  "#disable+" } {
                set enable 0
            }
#           set ok 3 
        } elseif {$tline eq ""} { ;# and don't want it for blank lines either 
#           set ok 4
        } elseif {[string index [string trim $tline] end] eq "\\"} { 
            # this actually shouldn't happen, parser removes them in info body, and so our line numbers
            # will also be correct if there's an error dialog, but we'll leave this code in anyway, wont hurt
            set ok 5
        } else {
            set  words [split $tline]
            set cmd [lindex $words 0]
            if { $cmd eq "return" || $cmd eq "lbp+"} {
                set ok 6
            }
        }
        if {  $ok == 0} { 
            # if it's still 0, we have an incomplete statement, but not a comment, blank line, or continuation (see other comments on this)
            # however, for incomplete lines that are in the style I use, we'll add a breakpoint after the opening brace, 
            # at the end of a line, (like on this if statement) for the several sorts of statements that have these
            # also add one after the closing brace, if the only one on the line
            set  words [split $tline]
            set cmd [lindex $words 0]
            if { $cmd eq "switch" } {
#               set norb 1 ;# if there's a switch statement seen, we apply the -norb option for here on
            }
            set rb $rbracket
            if { $norb } {
                set rb "xxxxxxxxxxxxxxxxxxxxxxxx"
            }
            set keys [list if foreach proc for while  $rb ]
            if {$cmd  in $keys } {
                if { [string index $tline end] eq $lbracket} {
                    set ok 2 ;# some sort of control command ending in a open brace
                }
            }
        }
        incr idn 10000
        set id "bpid$idn"
        if       { $ok == 0 || $ok == 3 || $ok == 4 || $ok == 5 || $ok == 6} { ;# we only gave these differnt numbers for debugging use
            set instrument "" ;# if incomplete or one blank lines, comment only, a return, and line continuation (but shouldn't see that here) 
        } elseif { $ok == 2 || $ok == 1 } {
            
            set instrument ";lbp+ step-instrument $id" ;# here's what we append, note step-instrument is also looked for to strip these when listing code
        } elseif { 0 } {
            
        } else {
            set instrument "" ;# should not ever get here, but if we ever do, just don't instrument this line
        }
        if { $ln == $nlines } {
            set instrument ""
        }
        if { $tline eq $rbracket && $norb} {
            set instrument ""
        } else {
#           set zzz [regsub  "([ \t]);##([ \t])" line {\1 $} ?varName?]
            set zzz [regsub -nocase -linestop -lineanchor {([ \t])(;#+)([ \t])(?!.*")} $line "\\1$instrument\\3\\2 " result]
            if { $zzz == 1 && $enable} {
                set line $result
                set instrument ""
            }
            if { $instrument ne "" } {
                incr idn ;# if we are doing an instrument, incr in lower portion also, for debugging mostly
            }
        }
        if { $enable } {
            if { $enable_seen } {
                append out "$instrument ; $line \n"
            } else {
                set trline [string trimleft $line]
                if { [string index $trline 0] eq "#"} {
                    append out "$instrument ;# instrument-show-begin $line ;# instrument-show-end\n"
                } else {
                    append out "$line  $instrument\n"
                }
            }
#           append out "$line $instrument\n"
        } else {
            append out "$line\n"    
        }
    }
    append out "trace add execution $procedure enter \{$::___zz___(util+) tracer\}\n"
    append out "trace add execution $procedure leave \{$::___zz___(util+) tracerend\}\n"
    if { !$return_seen && $no_warn == 0} {
        puts stderr "instrument+ Warning, no return found in $procedure - add one or use -nowarn"
    }
    if { $noeval } {
        return $out
    }
    eval $out
    return ""
}


if { 00 } {

 trace add variable ::___zz___(bpnum) write "dtracer "

}




namespace eval vwdebug {
    variable cash
    variable the_choicex
    set cash(0) "inited"
    proc listinstr {args} {
        foreach item [array names ::___zz___ original,*] {
            set pr [lindex [split $item , ] end ]
            puts "Instrumented procedure = $pr "    
        }
        foreach item [array names ::___zz___ originalm,*] {
            set mt [lindex [split $item , ] end ]
            set c $::___zz___($item)
            puts "Instrumented method    = [format %-15s $mt] -class $c"    
        }
    }
    proc do_destroy {kind args} {
        variable cash
        if { $::___zz___(cache) } {
        # if label, clear
        # if entry, clear, remove textvariable unset readonly
            set w [lindex $args 0 ]
            if { $kind eq "Label" } {
                $w config -text "\u220E" ;# black square
            } else { ;# an entry
                set var [$w cget -textvariable]
                $w config -textvariable {} -state normal -bg white
                $w delete 0 end
#               $w insert 0 "\u2400"    
            }
#           vwait ::ffff
        } else {
            tailcall destroy {*}$args
        }
    }
    
    proc do_label {m args} {
        variable cash
        if { $::___zz___(cache) } {
            if [catch {
                label {*}$args  ;# if it already exists, just configure after error
            } err_code] {
                lassign $args name wid widv txt txtv
                $name config $wid $widv $txt $txtv
            }
        } else {
            tailcall label {*}$args
        }
    }
    
    
    
    
    proc do_entry {m args} {
        variable cash
        if { $::___zz___(cache) } {
            if [catch {
                entry {*}$args
                return
            } err_code] {
                lassign $args name wid widv txt txtv
                $name config $wid $widv $txt $txtv
            }
        } else {
            tailcall entry {*}$args
        }
    }
    proc do_bind {args} {
        variable cash
        if { $::___zz___(cache) } {
            lassign $args w b
            tailcall bind {*}$args
        } else {
            tailcall bind {*}$args
        }
    }
    proc do_grid {args} {
        variable cash
        if { $::___zz___(cache) } {
            tailcall grid {*}$args
        } else {
            tailcall grid {*}$args
        }

    }
    proc get_frames {} {
        set result {}
        for {set m 0} {$m > $::___zz___(max_frames)} {incr m -1} {
            if [catch {
                set up      [ info frame $m]
                if { [dict get $up type ] eq "proc"} {
                    set p {}
                    if       { [dict exists $up proc] } {
                        set p [dict get $up proc]
                    } elseif {  [dict exists $up method] &&  [dict exists $up class]} {
                        set mth [dict get $up method]
                        set cla [dict get $up class]
                        set p $cla-$mth
                    }
                    if { $p != {} } {
                        lappend result $p
                    }
                }
            } err_code] {
#               if { $err_code  ne ""} {
#               }
                
            }
        }
        
        return $result
    }
        
proc cputs {dest string} {
    if { [info commands console] eq "" } {
        puts $string
    } else {
    if { ! [info exist ::tk::my_color_set] } {
        set ::tk::my_color_set 1
        console eval {
        set back grey30
        set fore white
        # if font not specified, it will honor console's larger/smaller
        .console tag configure debug              -foreground black -selectbackground $back -selectforeground $fore
        .console tag configure normal             -foreground black -selectbackground $back -selectforeground $fore
        .console tag configure green              -foreground \#408f40 -background \#e8e8e8 -selectbackground $back -selectforeground $fore
        .console tag configure white              -foreground white -background black  -font {courier 14 bold} -selectbackground $back -selectforeground $fore
        .console tag configure yellowonblack      -foreground yellow -background black -font {courier 14 bold} -selectbackground $back -selectforeground $fore
        .console tag configure yellow             -foreground yellow -background red -selectbackground blue
        .console tag configure whiteonred         -foreground white -background red -font {courier 14 bold} -selectbackground black
        .console tag configure rederror           -foreground red -background grey85 -font {courier 18 bold italic} -selectbackground black
        .console tag configure red                -foreground red -font {courier 18} -selectbackground $back -selectforeground $fore
        }
    }
    console eval [list ::tk::ConsoleOutput $dest $string]
    }
}
proc {lw} {widget} {      # list a widget
    set w [$widget configure]
    foreach item $w {
        set opt [lindex $item 0]
        set val "---"
        catch {set val [$widget cget $opt]}
        set wid($opt) $val 
    }   
    #la wid
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
                puts "$leftt|"
            } else {
                set left [lindex $names $m]
                set right [lindex $names $m2]
                set leftt [format {%-20s %-20s} $left $wid($left)]
#               cputs green "[string length $leftt] $left $wid($left)\n"
                if { [string length $leftt] > 41 } {
                    set leftt [format {%s %s} $left $wid($left)]
                    set leftt [string range [format %-41s $leftt] 0 40]
                }
                set rightt [format {%-20s %-20s} $right $wid($right)]
                puts "$leftt|[string trimright $rightt]"
            }
        }
    } else {
        for {set m 0;set m2 [expr ( $m+$n2 )]} {$m < $n2} {incr m;incr m2} {
                set left [lindex $names $m]
                set right [lindex $names $m2]
                set leftt [format {%-20s %-20s} $left $wid($left)]
#               cputs green "[string length $leftt] $left $wid($left)\n"
                if { [string length $leftt] > 41 } {
                    set leftt [format {%s %s} $left $wid($left)]
                    set leftt [string range [format %-41s $leftt] 0 40]
                }
                set rightt [format {%-20s %-20s} $right $wid($right)]
                puts "$leftt|[string trimright $rightt]"
        }
    }
}
proc wtree_node_puts {args} {
    puts ""
    puts ""
    cputs green "$args\n"
    lw $args
#   catch {cputs green "[pack info $args]\n"}
    if [catch {
        set zzz [pack info $args]
        cputs green "pack: $zzz\n" 
    } err_code] {
        if [catch {
            set zzz [grid info $args]
            if { $zzz ne "" } {
                cputs green "grid: $zzz\n" 
            }
        } err_code] {
            puts stderr "No grid or pack info to be found for $args"
        }
    }
#   clipboard clear ; clipboard append $args
}
proc wtree_node_openclose {which} {
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
proc wtree {{root .} {level 0}} {
    set top .wtree_top
    if {  $level == 0} {
        package require BWidget
        catch {
            $top.sw.t delete [$top.sw.t nodes root]
            destroy $top
        }
        toplevel $top
        frame $top.f
        button $top.f.b1 -text Open -command {vwdebug::wtree_node_openclose open}
        button $top.f.b2 -text Close -command {vwdebug::wtree_node_openclose close}
        pack $top.f  -side top -fill x
        pack $top.f.b1 $top.f.b2 -side left -expand yes -fill both
        ScrolledWindow $top.sw
        pack $top.sw -fill both -expand 1 -side top
        Tree $top.sw.t  -deltay 25 -deltax 25 -padx 5 -borderwidth 8 -linesfill orange -padx 5
        #pack $top.sw.t
        $top.sw setwidget $top.sw.t   ;# Make ScrolledWindow manage the Tree widget
        update                ;# Process all UI events before moving on.
        $top.sw.t bindText <1> +vwdebug::wtree_node_puts
        $top.sw.t bindText <3> "+$top.sw.t opentree "
        set ::wtree_queued_inserts {}
        wm geom $top 466x326+52+52
    }
    set children [winfo children $root]
    set class [winfo class $root]
    set info ""
    if       { $class == "Button" } {
        set info [split [$root cget -text] \n]
    } elseif { $class == "TLabelframe" } {
        set info [split [$root cget -text] \n]
    } elseif { $class == "TButton" } {
        set info [split [$root cget -text] \n]
    } elseif { $class == "TEntry" } {
        set info "var: [$root cget -textvariable]"
    } elseif { $class == "TCheckbutton" } {
        set info [split [$root cget -text] \n]
    } elseif { $class == "TButton2" } {
        set info [split [$root cget -text] \n]
    } else {
    }
    #cputs normal "[string repeat " |  " $level]"
    #cputs green "$root"
    #cputs red  [split $root .]
    #cputs normal " - $class  $info\n"
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
            set tout [wtree $child [expr ( $level + 1 )]]
        }
    }
    if { $level == 0 } {
        foreach item $::wtree_queued_inserts {
            eval $item
        }
    }
}
###########################################################
## Procedure:  ltree
proc ltree {{root .} {level 0}} {                   # list widget tree
    set children [winfo children $root]
    set class [winfo class $root]
    set info ""
    if       { $class == "Button" } {
        set info [split [$root cget -text] \n]
    } elseif { $class == "TLabelframe" } {
        set info [split [$root cget -text] \n]
    } elseif { $class == "TButton" } {
        set info [split [$root cget -text] \n]
    } elseif { $class == "TEntry" } {
        set info "var: [$root cget -textvariable]"
    } elseif { $class == "TCheckbutton" } {
        set info [split [$root cget -text] \n]
    } elseif { $class == "TButton2" } {
        set info [split [$root cget -text] \n]
    } else {
    }
    cputs normal "[string repeat " |  " $level]"
    cputs green "$root"
    cputs normal " - $class  $info\n"
    if { $children == "" } {
        return $root
    } else {
        foreach child $children {
            set tout [ltree $child [expr ( $level + 1 )]]
        }
    }   
}
proc choose {w choices {start 0} {max 20} {kind ?}} {
    variable the_choicex
    unset -nocomplain the_choicex
    catch {destroy .p}
    menu .p -tearoff 0
    update ; # <===========================================================
    .p  add command -label "-none- [string range $kind 6 end]" -command "set ::vwdebug::the_choicex {-none-} " -font {consolas-bold 13}
    set n $start
    foreach choice  [lrange [lsort -dictionary $choices]  $start  [expr {   $start+$max-1   }]] {
        if { $choice eq "" } {
            continue
        }
        .p  add command -label "[format %2s [incr n]] $choice" -command "set ::vwdebug::the_choicex $choice " -font {consolas 11}
    }
    if { $start+$max < [llength $choices]} {
        .p  add command -label "-next-" -command "set ::vwdebug::the_choicex {-next-} " -font {consolas-bold 13}
    }
    set coords [lrange [split [wm geom .] +] 1 end]
    set zzz [tk_popup .p {*}$coords 1]
#                                                                                   update
    if {! [info exist ::vwdebug::the_choicex] } {
        vwait  ::vwdebug::the_choicex
    }
    return $::vwdebug::the_choicex
}
proc getchoice {w choices kind} {
    variable the_choicex
    if { [llength $choices] == 1 } {
        set ::vwdebug::the_choicex $choices
        return $choices
    }
    set next 0
    set max 20
    while { 1 } {
        choose $w $choices $next $max $kind
        wait 100
        if { ![info exist ::vwdebug::the_choicex] } {
            set ::vwdebug::the_choicex "-none-"
        }
        if       { $::vwdebug::the_choicex eq "-none-" } {
            set ::vwdebug::the_choicex ""
            break
        } elseif { $::vwdebug::the_choicex eq "-next-" } {
            incr next $max
            if { $next > [llength $choices] } {
                set ::vwdebug::the_choicex ""
                break
            }
            continue
        } else {
            break
        }
    }
    return $::vwdebug::the_choicex
}
proc wait { ms } {
    set uniq [incr ::__sleep__tmp__counter]
    set ::__sleep__tmp__$uniq 0
    after $ms set ::__sleep__tmp__$uniq 1
    vwait ::__sleep__tmp__$uniq
    unset ::__sleep__tmp__$uniq
}
proc chooseprocs {args} {
    set procs {}
    set prs [info procs ::*]
    set skips {}
    foreach skip $::___zz___(procnolist) {
        lappend skips   [string trimleft $skip ":"]
    }
    foreach pr0  $prs {
        set pr [string trimleft $pr0 ":"]
        if { [string trimleft $pr ":"] in $skips  } {
            continue
        }
        if { [regexp $::___zz___(procnolistre) $pr] } {
            continue
        }
        if { $args ne "" } {
            if {! [string match {*}$args $pr] } {
                continue
            }
        }
        if { [info exist ::___zz___(original,$pr)] } {
            lappend procs "+$::___zz___(arrow)$pr"
        } else {
            lappend procs "$pr"
        }
    }
#   ll [lsort -dictionary $procs ]
    set zzz [getchoice . $procs "?"]
    if { [string index $zzz 0] eq "+" } {
        set zzz [string range $zzz 2 end]
    }
    if { $zzz ne "" } {
        if [catch {
            instrument+ $zzz
            puts stderr "instrumented+ $zzz"
        } err_code] {
            if { [string match {*is already instrumented,*} $err_code] } {
                instrument+ $zzz -revert
            } else {
                puts stderr "error: $err_code"
            }
        }
    }
}

} ;# end namespace vwdebug
if { $::___zz___(tooltipsbuiltin) } {
# tooltip.tcl --
#
#       Balloon help
#
# Copyright (c) 1996-2007 Jeffrey Hobbs
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id: tooltip.tcl,v 1.16 2008/12/01 23:37:16 hobbs Exp $
#
# Initiated: 28 October 1996


package require Tk 8.4
package require msgcat

#------------------------------------------------------------------------
# PROCEDURE
#   tooltip::tooltip
#
# DESCRIPTION
#   Implements a tooltip (balloon help) system
#
# ARGUMENTS
#   tooltip <option> ?arg?
#
# clear ?pattern?
#   Stops the specified widgets (defaults to all) from showing tooltips
#
# delay ?millisecs?
#   Query or set the delay.  The delay is in milliseconds and must
#   be at least 50.  Returns the delay.
#
# disable OR off
#   Disables all tooltips.
#
# enable OR on
#   Enables tooltips for defined widgets.
#
# <widget> ?-index index? ?-items id? ?-tag tag? ?message?
#   If -index is specified, then <widget> is assumed to be a menu
#   and the index represents what index into the menu (either the
#   numerical index or the label) to associate the tooltip message with.
#   Tooltips do not appear for disabled menu items.
#   If -item is specified, then <widget> is assumed to be a listbox
#   or canvas and the itemId specifies one or more items.
#   If -tag is specified, then <widget> is assumed to be a text
#   and the tagId specifies a tag.
#   If message is {}, then the tooltip for that widget is removed.
#   The widget must exist prior to calling tooltip.  The current
#   tooltip message for <widget> is returned, if any.
#
# RETURNS: varies (see methods above)
#
# NAMESPACE & STATE
#   The namespace tooltip is used.
#   Control toplevel name via ::tooltip::wname.
#
# EXAMPLE USAGE:
#   tooltip .button "A Button"
#   tooltip .menu -index "Load" "Loads a file"
#
#------------------------------------------------------------------------

namespace eval ::tooltip {
    namespace export -clear tooltip
    variable labelOpts
    variable tooltip
    variable G

    if {![info exists G]} {
        array set G {
            enabled     1
            fade        1
            FADESTEP    0.2
            FADEID      {}
            DELAY       500
            AFTERID     {}
            LAST        -1
            TOPLEVEL    .__tooltip__
        }
        if {[tk windowingsystem] eq "x11"} {
            set G(fade) 0 ; # don't fade by default on X11
        }
    }
    if {![info exists labelOpts]} {
    # Undocumented variable that allows users to extend / override
    # label creation options.  Must be set prior to first registry
    # of a tooltip, or destroy $::tooltip::G(TOPLEVEL) first.
    set labelOpts [list -highlightthickness 0 -relief solid -bd 1 \
               -background lightyellow -fg black]
    }

    # The extra ::hide call in <Enter> is necessary to catch moving to
    # child widgets where the <Leave> event won't be generated
    bind Tooltip <Enter> [namespace code {
    #tooltip::hide
    variable tooltip
    variable G
    set G(LAST) -1
    if {$G(enabled) && [info exists tooltip(%W)]} {
        set G(AFTERID) \
        [after $G(DELAY) [namespace code [list show %W $tooltip(%W) cursor]]]
    }
    }]

    bind Menu <<MenuSelect>>    [namespace code { menuMotion %W }]
    bind Tooltip <Leave>    [namespace code [list hide 1]] ; # fade ok
    bind Tooltip <Any-KeyPress> [namespace code hide]
    bind Tooltip <Any-Button>   [namespace code hide]
}

proc ::tooltip::tooltip {w args} {
    variable tooltip
    variable G
    switch -- $w {
    clear   {
        if {[llength $args]==0} { set args .* }
        clear $args
    }
    delay   {
        if {[llength $args]} {
        if {![string is integer -strict $args] || $args<50} {
            return -code error "tooltip delay must be an\
                integer greater than 50 (delay is in millisecs)"
        }
        return [set G(DELAY) $args]
        } else {
        return $G(DELAY)
        }
    }
    fade    {
        if {[llength $args]} {
        set G(fade) [string is true -strict [lindex $args 0]]
        }
        return $G(fade)
    }
    off - disable   {
        set G(enabled) 0
        hide
    }
    on - enable {
        set G(enabled) 1
    }
    default {
        set i $w
        if {[llength $args]} {
        set i [uplevel 1 [namespace code "register [list $w] $args"]]
        }
        set b $G(TOPLEVEL)
        if {![winfo exists $b]} {
        variable labelOpts

        toplevel $b -class Tooltip
        if {[tk windowingsystem] eq "aqua"} {
            ::tk::unsupported::MacWindowStyle style $b help none
        } else {
            wm overrideredirect $b 1
        }
        catch {wm attributes $b -topmost 1}
        # avoid the blink issue with 1 to <1 alpha on Windows
        catch {wm attributes $b -alpha 0.99}
        wm positionfrom $b program
        wm withdraw $b
        eval [linsert $labelOpts 0 label $b.label]
        pack $b.label -ipadx 1
        }
        if {[info exists tooltip($i)]} { return $tooltip($i) }
    }
    }
}

proc ::tooltip::register {w args} {
    variable tooltip
    set key [lindex $args 0]
    while {[string match -* $key]} {
    switch -- $key {
        -- {
            set args [lreplace $args 0 0]
            set key [lindex $args 0]
            break
        }
        -index {
        if {[catch {$w entrycget 1 -label}]} {
            return -code error "widget \"$w\" does not seem to be a\
                menu, which is required for the -index switch"
        }
        set index [lindex $args 1]
        set args [lreplace $args 0 1]
        }
        -item - -items {
                if {[winfo class $w] eq "Listbox"} {
                    set items [lindex $args 1]
                } else {
                    set namedItem [lindex $args 1]
                    if {[catch {$w find withtag $namedItem} items]} {
                        return -code error "widget \"$w\" is not a canvas, or\
                item \"$namedItem\" does not exist in the canvas"
                    }
                }
        set args [lreplace $args 0 1]
        }
            -tag {
                set tag [lindex $args 1]
                set r [catch {lsearch -exact [$w tag names] $tag} ndx]
                if {$r || $ndx == -1} {
                    return -code error "widget \"$w\" is not a text widget or\
                        \"$tag\" is not a text tag"
                }
                set args [lreplace $args 0 1]
            }
        default {
        return -code error "unknown option \"$key\":\
            should be -index, -items, -tag or --"
        }
    }
    set key [lindex $args 0]
    }
    if {[llength $args] != 1} {
    return -code error "wrong # args: should be \"tooltip widget\
        ?-index index? ?-items item? ?-tag tag? ?--? message\""
    }
    if {$key eq ""} {
    clear $w
    } else {
    if {![winfo exists $w]} {
        return -code error "bad window path name \"$w\""
    }
    if {[info exists index]} {
        set tooltip($w,$index) $key
        return $w,$index
    } elseif {[info exists items]} {
        foreach item $items {
        set tooltip($w,$item) $key
        if {[winfo class $w] eq "Listbox"} {
            enableListbox $w $item
        } else {
            enableCanvas $w $item
        }
        }
        # Only need to return the first item for the purposes of
        # how this is called
        return $w,[lindex $items 0]
        } elseif {[info exists tag]} {
            set tooltip($w,t_$tag) $key
            enableTag $w $tag
            return $w,$tag
    } else {
        set tooltip($w) $key
        # Note: Add the necessary bindings only once.
        set tags [bindtags $w]
        if {[lsearch -exact $tags "Tooltip"] == -1} {
        bindtags $w [linsert $tags end "Tooltip"]
        }
        return $w
    }
    }
}

proc ::tooltip::clear {{pattern .*}} {
    variable tooltip
    # cache the current widget at pointer
    set ptrw [winfo containing [winfo pointerx .] [winfo pointery .]]
    foreach w [array names tooltip $pattern] {
    unset tooltip($w)
    if {[winfo exists $w]} {
        set tags [bindtags $w]
        if {[set i [lsearch -exact $tags "Tooltip"]] != -1} {
        bindtags $w [lreplace $tags $i $i]
        }
        ## We don't remove TooltipMenu because there
        ## might be other indices that use it

        # Withdraw the tooltip if we clear the current contained item
        if {$ptrw eq $w} { hide }
    }
    }
}

proc ::tooltip::show {w msg {i {}}} {
    if {![winfo exists $w]} { return }

    # Use string match to allow that the help will be shown when
    # the pointer is in any child of the desired widget
    if {([winfo class $w] ne "Menu")
    && ![string match $w* [eval [list winfo containing] \
                   [winfo pointerxy $w]]]} {
    return
    }

    variable G

    after cancel $G(FADEID)
    set b $G(TOPLEVEL)
    # Use late-binding msgcat (lazy translation) to support programs
    # that allow on-the-fly l10n changes
    $b.label configure -text [::msgcat::mc $msg] -justify left
    update idletasks
    set screenw [winfo screenwidth $w]
    set screenh [winfo screenheight $w]
    set reqw [winfo reqwidth $b]
    set reqh [winfo reqheight $b]
    # When adjusting for being on the screen boundary, check that we are
    # near the "edge" already, as Tk handles multiple monitors oddly
    if {$i eq "cursor"} {
    set y [expr {[winfo pointery $w]+20}]
    if {($y < $screenh) && ($y+$reqh) > $screenh} {
        set y [expr {[winfo pointery $w]-$reqh-5}]
    }
    } elseif {$i ne ""} {
    set y [expr {[winfo rooty $w]+[winfo vrooty $w]+[$w yposition $i]+25}]
    if {($y < $screenh) && ($y+$reqh) > $screenh} {
        # show above if we would be offscreen
        set y [expr {[winfo rooty $w]+[$w yposition $i]-$reqh-5}]
    }
    } else {
    set y [expr {[winfo rooty $w]+[winfo vrooty $w]+[winfo height $w]+5}]
    if {($y < $screenh) && ($y+$reqh) > $screenh} {
        # show above if we would be offscreen
        set y [expr {[winfo rooty $w]-$reqh-5}]
    }
    }
    if {$i eq "cursor"} {
    set x [winfo pointerx $w]
    } else {
    set x [expr {[winfo rootx $w]+[winfo vrootx $w]+
             ([winfo width $w]-$reqw)/2}]
    }
    # only readjust when we would appear right on the screen edge
    if {$x<0 && ($x+$reqw)>0} {
    set x 0
    } elseif {($x < $screenw) && ($x+$reqw) > $screenw} {
    set x [expr {$screenw-$reqw}]
    }
    if {[tk windowingsystem] eq "aqua"} {
    set focus [focus]
    }
    # avoid the blink issue with 1 to <1 alpha on Windows, watch half-fading
    catch {wm attributes $b -alpha 0.99}
    wm geometry $b +$x+$y
    wm deiconify $b
    raise $b
    if {[tk windowingsystem] eq "aqua" && $focus ne ""} {
    # Aqua's help window steals focus on display
    after idle [list focus -force $focus]
    }
}

proc ::tooltip::menuMotion {w} {
    variable G

    if {$G(enabled)} {
    variable tooltip

        # Menu events come from a funny path, map to the real path.
        set m [string map {"#" "."} [winfo name $w]]
    set cur [$w index active]

    # The next two lines (all uses of LAST) are necessary until the
    # <<MenuSelect>> event is properly coded for Unix/(Windows)?
    if {$cur == $G(LAST)} return
    set G(LAST) $cur
    # a little inlining - this is :hide
    after cancel $G(AFTERID)
    catch {wm withdraw $G(TOPLEVEL)}
    if {[info exists tooltip($m,$cur)] || \
        (![catch {$w entrycget $cur -label} cur] && \
        [info exists tooltip($m,$cur)])} {
        set G(AFTERID) [after $G(DELAY) \
            [namespace code [list show $w $tooltip($m,$cur) cursor]]]
    }
    }
}

proc ::tooltip::hide {{fadeOk 0}} {
    variable G

    after cancel $G(AFTERID)
    after cancel $G(FADEID)
    if {$fadeOk && $G(fade)} {
    fade $G(TOPLEVEL) $G(FADESTEP)
    } else {
    catch {wm withdraw $G(TOPLEVEL)}
    }
}

proc ::tooltip::fade {w step} {
    if {[catch {wm attributes $w -alpha} alpha] || $alpha <= 0.0} {
        catch { wm withdraw $w }
        catch { wm attributes $w -alpha 0.99 }
    } else {
    variable G
        wm attributes $w -alpha [expr {$alpha-$step}]
        set G(FADEID) [after 50 [namespace code [list fade $w $step]]]
    }
}

proc ::tooltip::wname {{w {}}} {
    variable G
    if {[llength [info level 0]] > 1} {
    # $w specified
    if {$w ne $G(TOPLEVEL)} {
        hide
        destroy $G(TOPLEVEL)
        set G(TOPLEVEL) $w
    }
    }
    return $G(TOPLEVEL)
}

proc ::tooltip::listitemTip {w x y} {
    variable tooltip
    variable G

    set G(LAST) -1
    set item [$w index @$x,$y]
    if {$G(enabled) && [info exists tooltip($w,$item)]} {
    set G(AFTERID) [after $G(DELAY) \
        [namespace code [list show $w $tooltip($w,$item) cursor]]]
    }
}

# Handle the lack of <Enter>/<Leave> between listbox items using <Motion>
proc ::tooltip::listitemMotion {w x y} {
    variable tooltip
    variable G
    if {$G(enabled)} {
        set item [$w index @$x,$y]
        if {$item ne $G(LAST)} {
            set G(LAST) $item
            after cancel $G(AFTERID)
            catch {wm withdraw $G(TOPLEVEL)}
            if {[info exists tooltip($w,$item)]} {
                set G(AFTERID) [after $G(DELAY) \
                   [namespace code [list show $w $tooltip($w,$item) cursor]]]
            }
        }
    }
}

# Initialize tooltip events for Listbox widgets
proc ::tooltip::enableListbox {w args} {
    if {[string match *listitemTip* [bind $w <Enter>]]} { return }
    bind $w <Enter> +[namespace code [list listitemTip %W %x %y]]
    bind $w <Motion> +[namespace code [list listitemMotion %W %x %y]]
    bind $w <Leave> +[namespace code [list hide 1]] ; # fade ok
    bind $w <Any-KeyPress> +[namespace code hide]
    bind $w <Any-Button> +[namespace code hide]
}

proc ::tooltip::itemTip {w args} {
    variable tooltip
    variable G

    set G(LAST) -1
    set item [$w find withtag current]
    if {$G(enabled) && [info exists tooltip($w,$item)]} {
    set G(AFTERID) [after $G(DELAY) \
        [namespace code [list show $w $tooltip($w,$item) cursor]]]
    }
}

proc ::tooltip::enableCanvas {w args} {
    if {[string match *itemTip* [$w bind all <Enter>]]} { return }
    $w bind all <Enter> +[namespace code [list itemTip $w]]
    $w bind all <Leave> +[namespace code [list hide 1]] ; # fade ok
    $w bind all <Any-KeyPress> +[namespace code hide]
    $w bind all <Any-Button> +[namespace code hide]
}

proc ::tooltip::tagTip {w tag} {
    variable tooltip
    variable G
    set G(LAST) -1
    if {$G(enabled) && [info exists tooltip($w,t_$tag)]} {
        if {[info exists G(AFTERID)]} { after cancel $G(AFTERID) }
        set G(AFTERID) [after $G(DELAY) \
            [namespace code [list show $w $tooltip($w,t_$tag) cursor]]]
    }
}

proc ::tooltip::enableTag {w tag} {
    if {[string match *tagTip* [$w tag bind $tag]]} { return }
    $w tag bind $tag <Enter> +[namespace code [list tagTip $w $tag]]
    $w tag bind $tag <Leave> +[namespace code [list hide 1]] ; # fade ok
    $w tag bind $tag <Any-KeyPress> +[namespace code hide]
    $w tag bind $tag <Any-Button> +[namespace code hide]
}

package provide tooltip 1.4.6
}
if { 0 } { ;# little standalone tester even worked in 8.7 nightly
    lappend auto_path C:/tclf/tclpkgs
    if [catch {
        console show
    } err_code] {
        if [catch {
            source console.tcl
        } err_code] {
            puts stderr $err_code
        }
    }
    proc tester1 {args} {
        for {set n 0} {$n < 10 } {incr n} {
            set toobig($n) $n   
            set foo bar
            set foo bar 
        }
        puts hi1
        puts hi2
        puts hi3
        puts hi4
        if { 1 } {
            set one 1
        } else {
            set one 0   
        }
        set foo foo
        set bar bar
        return
    }
    proc tester2 {args} {
        puts hi1
        puts hi2
        puts hi3
        puts hi4
        for {set n 0} {$n < 10 } {incr n} {
            set toobig($n) $n
            set foo bar
            set foo bar 
        }
        if { 1 } {
            set one 1
        } else {
            set one 0   
        }
        set foo foo
        set bar bar
        return
    }
    instrument+ tester1
    instrument+ tester2
    tester1
    tester2
    tester1
    tester2
    tester1
    tester2
    tester1
    tester2
    tester1
    tester2
    tester1
    tester2
    tester1
    tester2
}
