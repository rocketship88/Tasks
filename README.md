# Tasks
Tasks is a Tcl/Tk add on to the tcl threads package to simply using threads

Tasks are an approach to writing multi-threaded script code using the powerful and dynamic Tcl programming language. Tasks make concurrency as simple as writing a procedure [proc] with a standard arglist. 

Based on techniques and code from the highly acclaimed book "The Tcl Programming Language" by Ashok Nadkarni, Tasks can take an independent procedure and transform it into a multi-threaded server. It is especially useful for compute intensive math functions that need to be called multiple times. Tasks mostly implement a sequential coding model, but are also compatible with the threads package event driven style. 

The Tasks arglist approach considerably reduces the need to learn new concepts. If you've wanted to dive into the Tcl threads package but haven't had the time to study the manual, then Tasks might be just the ticket for you.

Tasks provide [Tproc] an extension to the proc command. The below example will concurrently compute the number of digits of 5 (rather large) fibonacci numbers faster than if done sequentially when run on a multicore cpu: 

(example derived from Ashok's Promises blog) 

#----------------------------------------
# source the code 
# or copy to a module file (e.g. tasks-1.12.tm)
# and do a package require tasks

namespace import tasks::*
  
Tproc fibsize {number} {
    set fibnum [math::fibonacci $number]
    string length $fibnum
} -tasks 5 [list {-package require math} ]

tgroup fibsize -run 100000 100100 100200 100300 100400
 
parray fibsize rvar,* 
#----------------------------------------

tgroup runs 5 tasks (threads) concurrently and delivers the results to the array fibsize (* => 0..4 in arglist order). [parray] then outputs the results. 


Tasks include development tools and a simple Tk console for debugging. Tasks are source-able pure tcl. The wiki page at 

https://wiki.tcl-lang.org/page/Tasks

provides all the code, documentation, examples and youtube links to videos with intros and in-depth code walkthroughs to get you going with Tasks. The code discussion block has all the code that can be copied/pasted into a file and sourced. 
 
Cpu core counts are on the rise. Tcl Tasks can gently ease one into concurrent programming to better utilize these new machines.


