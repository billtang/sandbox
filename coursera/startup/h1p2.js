#!/usr/bin/env node
var fs = require('fs');
var outfile = "primes.txt";

var primes = new Array(); //[ 2, 3, 5, 7, 11, 13, 17, 19, 23];
primes.push(2);
primes.push(3);
var number = 5;
var count = 2;
var found = 1;

var flag = 0;
while(count <= 100) {
    for (var i = 0; i < primes.length; i++) {
	var candidate = primes[i];
	console.log("checking " + number.toString() 
		    + " % " + candidate.toString());
	if ( number % candidate == 0) {
	    console.log("reject " + number.toString()
			+ " % " + candidate.toString());
	    flag = -1;
	    break;
	}

    }
    if (flag == -1) {
    } else {
	console.log("found " + number.toString());
	primes.push( number );
	count = count + 1;
    }
    number = number + 2; flag = 0;
}
var out = "";
for (var i = 0; i < primes.length; i++) {
    if (out.length > 0) {out = out + "," + primes[i].toString();}
    else {out = primes[i].toString();}
}
fs.writeFileSync(outfile, out);
console.log(out);
