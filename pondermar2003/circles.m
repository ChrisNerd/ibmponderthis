function returnvalue = circles(inputargs)
  %[aandb area] = fminsearch(@circles,[15 -20])
  %if you have the optimization toolbox
  
  a=inputargs(1);
  b=inputargs(2);

  if(1)
    returnvalue=-area(a,b);
    return
  end
  
  delta = 1e-0;
  for itnumber=1:5
    areaprime = area(a,b);
    plotarea(itnumber)=areaprime;
    plota(itnumber)=a;
    plotb(itnumber)=b;

    a2 = a -delta* dAda(a,b)/(dAda(a+delta,b)-dAda(a,b));
    b = b -delta* dAdb(a,b)/(dAdb(a,b+delta)-dAdb(a,b));
    a=a2;
  end
  subplot(211)
  plot(plotarea);
  subplot(212)
  plot(plota,plotb);
    

  
function returnvalue = solvesinc(y,l2);
  x=.8;
  delta = 1e-4;
  %y*pi/l2;
  count = 0;
  while (abs(sinc(x)-y/l2) > 1e-12)
    count = count +1;
    if(sinc(x+delta) == sinc(x) | count > 200)
      %      keyboard;
      count
      returnvalue = -1;
      return
    end
    x = x - delta*(sinc(x)-y/l2)/(sinc(x+delta)-sinc(x));
  end
  returnvalue=x;
  
    
function returnvalue = dAda(a,b)
  delta=1e-3;
  returnvalue =  (area(a+delta,b)-area(a,b))/delta;
function returnvalue = dAdb(a,b)
  delta=1e-3;
  returnvalue =  (area(a,b+delta)-area(a,b))/delta;

  
function returnvalue = area(a,b)
  r1=sqrt(a^2+b^2);
  y=solvequadratic(1, -2*b, 20^2 -2*20*a);
  %a
  %b
  %sqrt((a-20)^2+(b-y)^2)
  
  
  if (y < 0)
    a;
    b;
    returnvalue = 0;
    return
  end
  %theta1 = 2* acos((a*(a-20)+b^2)/(r1*sqrt((a-20)^2+b^2)))
  theta1 = acos((20^2+y^2-2*r1^2)/(-2*r1^2)) ;
  l1 = r1*theta1;
  l2 = 50-l1;
    
  areaofquad1 = 20*-b/2 + y*(a-20)/2;
  areaofinterest1 = r1^2*theta1/2 - areaofquad1;
    
  %function make = 0 ..    sinc(x)-y/l2;
  %  y
  %  l2
  if (y > l2)
    returnvalue =0;
    return
  end
  
  sincreturn = solvesinc(y,l2);
  if (sincreturn == -1)
    returnvalue = 0;
    return
  end
  %  sincrecturn = fsolve(sinc(x)-y/l2,.7)
  
  theta2=sincreturn * pi;
  r2 = l2/theta2;
  
  x = 20 + sqrt(r2^2-y^2);
  
  areaofquad2 = 1/2*y*(x-20);
  areaofinterest2 = r2^2*theta2/2 - areaofquad2;
  
  
  returnvalue = areaofinterest1 + 2*areaofinterest2;
  
function returnvalue = solvequadratic(a,b,c)
  returnvalue = (-b + sqrt(b^2 -4*a*c))/(2*a);
