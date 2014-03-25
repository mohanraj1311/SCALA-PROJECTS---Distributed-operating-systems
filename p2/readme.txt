Name: Rajamohan Mohan Raj
UFID: 79894471
Team Member : 1
=================================================================================
What is working:

The use of scala actors to determine the convergence of two algorithms namely:
1. Gossip: In this algo, the 'masterClass' creates different topologies viz:line,full,2D and imp-2D,
and then starts to spread a 'rumor' to the randomly selected receiver actor.This randomly selected
actor again spreads the 'rumor' to the different terminals on the network.
Once the same receiver actor receives more than 10 times the same 'rumor' message it sends a 'stop'  message to the
sender actor and a 'done' message to the main actor created.
Once the main actor receives the 'done' message from all the terminals, it exits itself and we record the time for the same.

2. Push-Sum : This also works exactly the same way as above but in this case, instead of a 'rumor' its like we have an sValue and wValue associated
with each message sent across which we need to process. These values are processed by each of the actors when it receives or sends them.
While receiving it should add its own sVal and wVal and while sending it should send only the half on the values.
The receiver actor needs to check the difference b/w the the three consecutive pairs of ratios of sVal/wVal.
If the difference is less than 0.0000000001 then the termination begins as in the 'gossip' when we receive the 'stop'.


================================================================================

Largest Network Managed for each algorithm and topology.

This may vary significantly from machine to machine.

On my personal laptop, the values were as follows:

For Gossip:
-----------

Line:25
Full:100
2D:70
Imp-2D:70

For Push-Sum:
-------------
Line : 15
Full: 100
2D:40
imp-2D:40.

NOTE: The highest values are not recorded everytime i run the program.



