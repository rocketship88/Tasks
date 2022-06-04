# Tasks
Tasks is a Tcl/Tk add on to the tcl threads package to simplify using threads.

Tasks are an approach to writing multi-threaded script code using the powerful and dynamic Tcl programming language. Tasks make concurrency as simple as writing a procedure [proc] with a standard arglist. 

Based on techniques and code from the highly acclaimed book "The Tcl Programming Language" by Ashok Nadkarni, Tasks can take an independent procedure and transform it into a multi-threaded server. It is especially useful for compute intensive math functions that need to be called multiple times. Tasks mostly implement a sequential coding model, but are also compatible with the threads package event driven style. 

The Tasks arglist approach considerably reduces the need to learn new concepts. If you've wanted to dive into the Tcl threads package but haven't had the time to study the manual, then Tasks might be just the ticket for you.

Tasks provide [Tproc] an extension to the proc command. 

Tasks include development tools and a simple Tk console for debugging. Tasks are source-able pure tcl. The wiki page at 

https://wiki.tcl-lang.org/page/Tasks

provides all the documentation, examples and youtube links to videos with intros and in-depth code walkthroughs to get you going with Tasks.
 
Cpu core counts are on the rise. Tcl Tasks can gently ease one into concurrent programming to better utilize these new machines.


![image](https://user-images.githubusercontent.com/20431650/168942185-af1f62ca-323c-41f9-a243-4a20489c1ad5.png)
