#//// do event driven simulation
#// add events starts to priority_queue
N=4;
# starts = [{1, start}, {2, start}, .... 4] // =1:N
#types
t=zeros(1,N);

for i=1:N
 event(i).t = i; # t = is time
# b is for boolean, false if it is a start event, true if it is a collision event
 event(i).b = 0 ;
 speed(i) = rand();
end

# while priority_queue is not empty
#while (length (event) != 0)

# h = priority_queue.pop_head()
[minValue,index] = min([event.t])
#extract the bool
bo = event(index).b;
sp = event(index).speed
#delete the element
event(index) = [];
# if h is a start
if(!bo)
#/ find d that is the closest bullet that is slower than h
#/ if d exists
## add new event for collision between bullet and d
# if h is a collision
#/  anihilated bullets+=2
#/  delete the 2 bullets involved
## delete the potential collision involved with bullet #2
## delete the potential collision involved with bullet #1
## find first bullet > 2 that exists call it b3
## find last bullet < 1 that exists call it b0
## if both b3 and b0 exist and if b3 is faster than b0
#/// add new event for collision between 3 and 0





